;; Slowly migrating my zsh aliases here...

(uiop:define-package :fare-scripts/commands
  (:use :cl :uiop :fare-utils
        :inferior-shell :cl-scripting/failure :cl-launch/dispatch)
  (:export #:fare-scripts-symlinks #:register-command #:register-commands))

(in-package :fare-scripts/commands)

(exporting-definitions

(defun fare-scripts-symlinks ()
  (let ((binarch (resolve-absolute-location `(,(getenv "BINDIR") ,(getenv "BINARCH")) :ensure-directory t)))
    (with-current-directory (binarch)
      (dolist (i (cl-launch/dispatch:all-entry-names))
        (unless (file-exists-p i)
          (format t "linking file ~A~%" i)
          (run `(ln -s multi ,i))))))
  (success))

(defun register-command (command)
  (check-type command symbol)
  (when (fboundp command)
    (cl-launch/dispatch:register-entry
     (string-downcase command)
     #'(lambda (argv) (apply 'run-command command argv)))))

(defun register-commands (commands)
  (etypecase commands
    (list (map () 'register-command commands))
    ((or string symbol package) (do-external-symbols (command commands) (register-command command)))))

);exporting-definitions

;; Not all our exported symbols are worth exposing to the shell command-line.
(register-commands '(fare-scripts-symlinks))
