;; Slowly migrating my zsh aliases here...

(defpackage :fare-scripts/shell-aliases
  (:use :cl :fare-utils :uiop :inferior-shell :optima :optima.ppcre :cl-launch/dispatch))

(in-package :fare-scripts/shell-aliases)

(def*fun mkba ()
  (with-current-directory ((subpathname (user-homedir-pathname) "src/fare/bastiat.org/"))
    (run '(make dep)) (run '(make))))

(def*fun mkba2 ()
  (mkba) (mkba))

(def*fun fare-scripts-symlinks ()
  (let ((binarch (resolve-absolute-location `(,(getenv "BINDIR") ,(getenv "BINARCH")) :ensure-directory t)))
    (with-current-directory (binarch)
      (dolist (i (cl-launch/dispatch:all-entry-names))
        (unless (file-exists-p i)
          (format t "linking file ~A~%" i)
          (run `(ln -s multi ,i)))))))

(do-external-symbols (cmd :fare-scripts/shell-aliases)
  (cl-launch/dispatch:register-entry (string-downcase cmd) (lambda (argv) (apply cmd argv))))
