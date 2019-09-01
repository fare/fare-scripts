(uiop:define-package :fare-scripts/xrandr
  (:use :cl :uiop :fare-utils
        :optima :optima.ppcre
        :inferior-shell :cl-scripting :cl-launch/dispatch)
  (:export #:screen-device-up #:screen-device-right #:screen-device-down #:screen-device-left))

(in-package :fare-scripts/xrandr)

;; TODO: write a real parser for xrandr output?

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

(exporting-definitions

(defun screen-device-up (&optional (device (current-device)))
  (run/i `(xrandr --output ,device --rotate normal))
  (configure-touchscreen :invert-x nil :invert-y nil :swap-xy nil :matrix '(1 0 0  0 1 0  0 0 1)))
(defun screen-device-right (&optional (device (current-device)))
  (run/i `(xrandr --output ,device --rotate right))
  (configure-touchscreen :invert-x nil :invert-y t :swap-xy t :matrix '(0 1 0  -1 0 1  0 0 1)))
(defun screen-device-down (&optional (device (current-device)))
  (run/i `(xrandr --output ,device --rotate inverted))
  (configure-touchscreen :invert-x t :invert-y t :swap-xy nil :matrix '(-1 0 1  0 -1 1  0 0 1)))
(defun screen-device-left (&optional (device (current-device)))
  (run/i `(xrandr --output ,device --rotate left))
  (configure-touchscreen :invert-x t :invert-y nil :swap-xy t :matrix '(0 -1 1  1 0 0  0 0 1)))

);exporting-definitions

;; Not all our exported symbols are worth exposing to the shell command-line.
(register-commands :fare-scripts/xrandr)
