;; Personal scripts to deal with various programming languages

(uiop:define-package :fare-scripts/languages
  (:use :cl :fare-utils :uiop
   :inferior-shell :cl-scripting :fare-scripts/commands
   :optima :optima.ppcre
   :cl-launch/dispatch))

(in-package :fare-scripts/languages)

(exporting-definitions

(defun mkba ()
  (with-current-directory ((subpathname (src-root) "fare/bastiat.org/"))
    (run '(make dep)) (run '(make)))
  (success))

(defun mkba2 ()
  (mkba) (mkba))

(defun mygcl ()
  ;; git clone git://git.sv.gnu.org/gcl.git
  (with-current-directory ((subpathname (common-lisp-src) "gcl/gcl/"))
    (run/interactive `(git clean -xfd))
    (run/interactive `(./configure --enable-ansi (--prefix=,(stow-root)gcl))) ;; --disable-dynsysgmp --enable-static ???
    (run/interactive `(make -l6 install (prefix=,(stow-root)gcl)))
    (delete-file (subpathname (stow-root) "gcl/share/info/dir"))
    (success)))

(defun mysbcl ()
  (with-current-directory ((subpathname (common-lisp-src) "sbcl/"))
    (let ((install-root (subpathname (stow-root) "sbcl/"))
          (out (subpathname (temporary-directory) "mysbcl.out")))
      (run/interactive `(pipe ("sh" "./make.sh" ("--prefix=",install-root)
                                    "--xc-host=/usr/bin/sbcl --disable-debugger --no-userinit --no-sysinit"
                                    "--fancy") (tee ,out)))
      (ignore-errors
       (delete-directory-tree (subpathname install-root "lib/sbcl/") :validate #'(lambda (p) (subpathp p install-root))))
      (run/interactive `(pipe (sh "./install.sh" (--prefix=,install-root)) (tee -a ,out)))))
  (success))

(defun mysbcl-contrib (&optional tag)
  (let ((install-root (subpathname (stow-root) "sbcl/"))
        (out (subpathname (temporary-directory) "mysbcl-contrib.out")))
    (with-current-directory ((subpathname (common-lisp-src) "sbcl/obj/asdf-upstream/"))
      (run/interactive `(pipe (git reset --hard ,tag) (tee ,out)))
      (run/interactive `(pipe (git pull (subpathname (cl-root) "asdf/")) (tee -a ,out))))
    (with-current-directory ((subpathname (common-lisp-src) "sbcl/contrib/asdf/"))
      (run/interactive `(pipe (make up "SBCL=../../run-sbcl.sh") (tee -a ,out))))
    (with-current-directory ((subpathname (common-lisp-src) "sbcl/"))
      (run/interactive `(pipe (sh "./make-target-contrib.sh" (--prefix=,install-root) --with-sb-core-compression)) (tee -a out))
      (run/interactive `(pipe (sh "./install.sh" (--prefix=,install-root)) (tee -a out)))))
  (success))

(defun myrust ()
  (with-current-directory ((subpathname (src-root) "proglang/rust/"))
    (let ((install-root (subpathname (stow-root) "rust/"))
          (out (subpathname (temporary-directory) "myrust.out")))
      (run/interactive `(pipe (sh "./configure" (--prefix=,install-root)) (tee ,out)))
      ;;(delete-directory-tree (subpathname install-root "lib/rust/") :validate #'(lambda (p) (subpathp p install-root)))
      (run/interactive `(pipe (make install) (tee -a ,out)))))
  (success))

);exporting-definitions

(register-commands :fare-scripts/languages)
