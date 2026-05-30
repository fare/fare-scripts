":" ; exec cl-launch -Q -sm fare-scripts/toggle-touchpad "$0" "$@"
;; -*- lisp -*-
;; Based on https://wiki.archlinux.org/index.php/Touchpad_Synaptics#Software_toggle
;; Use the UI preferences to add a keyboard shortcut that invokes this script.
;; To avoid the slow startup time of lisp as a script, better dump an image with:
;;   cl-launch -o ~/bin/x64/toggle-touchpad -d ! -l clisp \
;;     -s optima.ppcre -s inferior-shell -E toggle-touchpad::main -L toggle-touchpad.lisp
;; Or use make-multi.sh to create a multi-call binary that includes toggle-touchpad support.

(uiop:define-package :fare-scripts/toggle-touchpad
  (:use :cl :fare-utils :uiop :inferior-shell
        :optima :optima.ppcre :cl-scripting)
  (:export #:help #:get-touchpad-id #:touchpad-enabled-p
           #:toggle-touchpad #:disable-touchpad #:enable-touchpad))

(in-package :fare-scripts/toggle-touchpad)

(defun get-touchpad-id ()
  (dolist (line (run/lines '(xinput list)))
    (match line
      ((ppcre "(TouchPad|\\sSYNA.*|Synaptics\\s.*|SynPS/2 Synaptics TouchPad)\\s+id\=([0-9]{1,2})\\s+" _ x)
       (return (values (parse-integer x)))))))

(defun touchpad-enabled-p (&optional (id (get-touchpad-id)))
  (dolist (line (run/lines `(xinput list-props ,id)))
    (match line
      ((ppcre "Touchpad Enabled\\s+[():0-9]+\\s+([01])" x) (return (equal x "1"))))))

(defun toggle-touchpad (&optional (id (get-touchpad-id)) (on :toggle))
  (let ((state (ecase on
                 ((:toggle) (not (touchpad-enabled-p id)))
                 ((nil t) on))))
    (run `(xinput ,(if state 'enable 'disable) ,id)))
  (success))

(defun enable-touchpad (&optional (id (get-touchpad-id)))
  "Enable touchpad"
  (toggle-touchpad id t))

(defun disable-touchpad (&optional (id (get-touchpad-id)))
  "Disable touchpad"
  (toggle-touchpad id nil))

(defun help (&optional (output *standard-output*))
  (format output "toggle-touchpad functions: ~{~(~A~)~^ ~}~%"
          (package-functions :fare-scripts/toggle-touchpad))
  (success))

(defun main (argv) ;; TODO: use command-line-arguments, or CLON
  (cond
    ((null argv) (toggle-touchpad))
    ((eql (first-char (first argv)) #\() (eval (first argv)))
    (t (if-let (fun (package-function :fare-scripts/toggle-touchpad
                                      (standard-case-symbol-name (first argv))))
         (apply 'run-command fun (rest argv))
         (progn
           (format *error-output* "Bad toggle-touchpad command: ~A~%" (first argv))
           (help *error-output*)
           (quit 2))))))
