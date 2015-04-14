":" ; exec cl-launch -s optima.ppcre -s inferior-shell -E fare-scripts/toggle-touchpad:main "$0" "$@"
;; -*- lisp -*-
;; Based on https://wiki.archlinux.org/index.php/Touchpad_Synaptics#Software_toggle
;; Use the UI preferences to add a keyboard shortcut that invokes this script.
;; To avoid the slow startup time of lisp as a script, better dump an image with:
;;   cl-launch -o ~/bin/x64/toggle-touchpad -d ! -l clisp \
;;     -s optima.ppcre -s inferior-shell -E toggle-touchpad::main -L toggle-touchpad.lisp
;; Or use make-multi.sh to create a multi-call binary that includes toggle-touchpad support.

(defpackage :fare-scripts/toggle-touchpad
  (:use :cl :uiop :inferior-shell :optima :optima.ppcre)
  (:export #:main #:get-touchpad-id #:device-enabled-p
           #:toggle-device #:disable-device #:enable-device))

(in-package :fare-scripts/toggle-touchpad)

(defun get-touchpad-id ()
  (dolist (line (run/lines '(xinput list)))
    (match line
      ((ppcre "ouchPad\\s*id\=([0-9]{1,2})" x) (return (values (parse-integer x)))))))

(defun device-enabled-p (&optional (id (get-touchpad-id)))
  (dolist (line (run/lines `(xinput list-props ,id)))
    (match line
      ((ppcre "Device Enabled\\s+[():0-9]+\\s+([01])" x) (return (equal x "1"))))))

(defun toggle-device (&optional (id (get-touchpad-id)) (state (not (device-enabled-p id))))
  (run `(xinput ,(if state 'enable 'disable) ,id)))

(defun enable-device (&optional (id (get-touchpad-id)))
  (toggle-device id t))

(defun disable-device (&optional (id (get-touchpad-id)))
  (toggle-device id nil))

(defun main (argv)
  (cond
    ((null argv) (toggle-device))
    ((eql (first-char (first argv)) #\() (eval (first argv)))
    (t (apply (find-symbol (string-upcase (first argv))) (rest argv)))))
