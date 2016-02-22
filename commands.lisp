;; Slowly migrating my zsh aliases here...

(uiop:define-package :fare-scripts/commands
  (:use :cl :uiop :fare-utils
        :inferior-shell :cl-scripting :cl-launch/dispatch)
  (:export #:fare-dir #:src-root #:common-lisp-src #:cl-root
           #:getuid #:stow-root #:restow
           #:fare-scripts-symlinks #:help))

(in-package :fare-scripts/commands)

(exporting-definitions

(defun fare-dir () (getenv-absolute-directory "FARE"))
(defun src-root () (subpathname (fare-dir) "src/"))
(defun common-lisp-src () (subpathname (src-root) "common-lisp/"))
(defun cl-root () (subpathname (fare-dir) "cl/"))

(defun getuid ()
  #+sbcl (sb-posix:getuid)
  #-sbcl (error "no getuid")) ;; use iolib?

(defun stow-root ()
  (if (zerop (getuid))
      #p"/usr/local/stow/"
      (subpathname (fare-dir) "local/stow/")))

(defun restow ()
  (with-current-directory ((stow-root))
    (run `(stow "-R" ,@(mapcar (lambda (x) (car (last (pathname-directory x)))) (subdirectories "."))))
    (run '(symlinks -rd "..")))
  (success))

(defun fare-scripts-symlinks ()
  (let ((binarch (resolve-absolute-location `(,(getenv "BINDIR") ,(getenv "BINARCH")) :ensure-directory t)))
    (with-current-directory (binarch)
      (dolist (i (cl-launch/dispatch:all-entry-names))
        (unless (file-exists-p i)
          (format t "linking file ~A~%" i)
          (run `(ln -s multi ,i))))))
  (success))

(defun help ()
  (format! t "~A available commands: ~{~A~^ ~}~%" (get-name) (all-entry-names))
  (success))
);exporting-definitions

;; Not all our exported symbols are worth exposing to the shell command-line.
(register-commands :fare-scripts/commands)
