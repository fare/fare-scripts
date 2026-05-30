(uiop:define-package :fare-scripts/video
  (:use :cl :uiop :fare-utils
        :optima :optima.ppcre
        :inferior-shell :cl-scripting :cl-launch/dispatch)
  (:export
   #:brightness-down #:brightness-up #:capture-screen #:lock-screen
   #:screen-up #:screen-right #:screen-down #:screen-left))

(in-package :fare-scripts/video)

(defun current-device () "eDP-1")

(defun xinput-device-properties (device-id)
  (loop :for line :in (cdr (run/lines `(xinput list-props ,device-id))) :collect
    (match line
      ((ppcre "^\\s+([A-Za-z-0-9][A-Za-z0-9 ]*[A-Za-z-0-9]) [(]([0-9]+)[)]:\\s+(.*)$"
              name id value)
       (list name (parse-integer id) value))
      (_ (error "Cannot parse device property line ~A" line)))))

(defun touchscreen-devices ()
  (while-collecting (c)
    (dolist (line (run/lines '(xinput list)))
      (match line
        ((ppcre "(ELAN21EF:00 04F3:[0-9A-F]{4}|TPPS/2 IBM TrackPoint|SynPS/2 Synaptics TouchPad|Wacom Co.,Ltd. Pen and multitouch sensor (Pen.*|Finger))\\s+id\=([0-9]{1,2})\\s+" _ _ x)
         (c (parse-integer x)))))))

(defun configure-touchscreen (&key invert-x invert-y swap-xy matrix)
  "Configure all builtin pointer devices to follow the given orientation.
INVERT-X, INVERT-Y and SWAP-XY specify how to configure the devices with the Evdev mechanism;
MATRIX specifies how to configure the devices with the Coordinate Transformation Matrix mechanism."
  (dolist (ts (touchscreen-devices))
    (if-let (properties (ignore-errors (xinput-device-properties ts)))
      (flet ((property-id (name) (second (find name properties :key 'first :test 'equal))))
        (if-let (c-t-m (property-id "Coordinate Transformation Matrix"))
          (run/i `(xinput set-prop ,ts ,c-t-m ,@matrix) :on-error nil)
          (if-let (axis-inversion (property-id "Evdev Axis Inversion"))
            (if-let (axes-swap (property-id "Evdev Axes Swap"))
              (progn
                (run/i `(xinput set-prop ,ts ,axis-inversion ,(if invert-x 1 0) ,(if invert-y 1 0)))
                (run/i `(xinput set-prop ,ts ,axes-swap ,(if swap-xy 1 0)))))))))))

;; TODO: make it work automatically on non-intel video cards.
(defparameter *brightness-path*
  (first (uiop:directory-files "/sys/class/backlight/" "*/brightness")))
(defparameter *max-brightness-path*
  (uiop:merge-pathnames* "max_brightness" *brightness-path*))
(defun get-brightness () (uiop:read-file-form *brightness-path*))
(defun get-max-brightness () (uiop:read-file-form *max-brightness-path*))


;;(defun set-brightness (b) (with-output-file (o *brightness-path*) (princ b o))) ;; must be done as root
;; TODO: instead, be using a logarithmic scale? 0, 1... 1060
;; (defun f (n) (round (1- (expt (1+ maxbri) (/ n 20))))) ;; for n from 1 to 20, because (f 0) = (f 1) = 0 ?
;; but then need to decompose current level?
;; (defun g (l) (* 20 (log (1+ l) (1+ maxbri)))) ;; <= bad behavior around 0 :-(

(defun set-brightness (b)
  (uiop:run-program `("sudo" "tee" ,(uiop:native-namestring *brightness-path*))
                    :input `(,(princ-to-string b)) :output t :error-output t :ignore-error-status t))

(defun fit-bounds (min max n)
  (cond
    ((< n min) min)
    ((> n max) max)
    (t n)))

(defun adjust-brightness (percent)
  (let* ((brightness (get-brightness))
         (max-brightness (get-max-brightness))
         (new-brightness (fit-bounds 0 max-brightness
                                     (+ brightness (round (* max-brightness 1/100 percent))))))
    (set-brightness new-brightness)
    (round (* 100 new-brightness) max-brightness)))

(exporting-definitions

(defun brightness-down ()
  "decrease brightness"
  ;;(run-shell-command "xbacklight -dec 5") "brightness down"
  (format nil "brightness down to ~A%" (adjust-brightness -5)))

(defun brightness-up ()
  "increase brightness"
  ;;(run-shell-command "xbacklight -inc 5") "brightness up"
  (format nil "brightness up to ~A%" (adjust-brightness +5)))

(defun capture-screen ()
  "Capture screen"
  (run/i `(scrot "%Y-%m-%d_$wx$h.png" -e "mv $f ~/DL/screencap/'")))

(defun lock-screen ()
  "Lock the screen"
  (run/i `(xscreensaver-command --lock)))


;; TODO: write a real parser for xrandr output?

(defun screen-up (&optional (device (current-device)))
  "Set device rotation facing up"
  (run/i `(xrandr --output ,device --rotate normal))
  (configure-touchscreen :invert-x nil :invert-y nil :swap-xy nil :matrix '(1 0 0  0 1 0  0 0 1)))

(defun screen-right (&optional (device (current-device)))
  "Set device rotation facing right"
  (run/i `(xrandr --output ,device --rotate right))
  (configure-touchscreen :invert-x nil :invert-y t :swap-xy t :matrix '(0 1 0  -1 0 1  0 0 1)))

(defun screen-down (&optional (device (current-device)))
  "Set device rotation facing down"
  (run/i `(xrandr --output ,device --rotate inverted))
  (configure-touchscreen :invert-x t :invert-y t :swap-xy nil :matrix '(-1 0 1  0 -1 1  0 0 1)))

(defun screen-left (&optional (device (current-device)))
  "Set device rotation facing left"
  (run/i `(xrandr --output ,device --rotate left))
  (configure-touchscreen :invert-x t :invert-y nil :swap-xy t :matrix '(0 -1 1  1 0 0  0 0 1)))

);exporting-definitions

(register-commands :fare-scripts/video)
