;; Migrating my zsh aliases here...

(defpackage :fare-scripts/shell-aliases
  (:use :cl :uiop :inferior-shell :optima :optima.ppcre :cl-launch/dispatch)
  (:export #:mkba #:mkba2 #:fare-scripts-symlinks))

(in-package :fare-scripts/shell-aliases)

(defun mkba ()
  (with-current-directory ((subpathname (user-homedir-pathname) "src/fare/bastiat.org/"))
    (run '(make dep)) (run '(make))))

(defun mkba2 ()
  (mkba) (mkba))

(defun fare-scripts-symlinks ()
  (let ((binarch (resolve-absolute-location `(,(getenv "BINDIR") ,(getenv "BINARCH")) :ensure-directory t)))
    (with-current-directory (binarch)
      (dolist (i (cl-launch/dispatch:all-entry-names))
        (unless (file-exists-p i)
          (format t "linking file ~A~%" i)
          (run `(ln -s multi ,i)))))))

(do-external-symbols (cmd :fare-scripts/shell-aliases)
  (cl-launch/dispatch:register-entry (string-downcase cmd) (lambda (argv) (apply cmd argv))))
