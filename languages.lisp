;; Personal scripts to deal with various programming languages

(uiop:define-package :fare-scripts/languages
  (:use :cl :fare-utils :uiop
   :inferior-shell :cl-scripting :fare-scripts/commands
   :optima :optima.ppcre
   :cl-launch/dispatch))

(in-package :fare-scripts/languages)

(exporting-definitions

(progn
  (defun nns (x)
    (let ((s (native-namestring x)))
      (if (and (equal (last-char s) #\/) (not (equal s "/")))
          (subseq s 0 (1- (length s)))
          s))))

(defun mkba ()
  (with-current-directory ((subpathname (src-root) "fare/bastiat.org/"))
    (run '(make dep)) (run '(make)))
  (success))

(defun mkba2 ()
  (mkba) (mkba))

(defun myclisp ()
  (with-current-directory ((subpathname (common-lisp-src) "clisp/"))
    (run/i `(hg clean))
    (ignore-errors
     (delete-directory-tree (subpathname (common-lisp-src) "clisp/build-dir/")))
    (run/i `(./configure
             --with-ffcall
             (--with-libffcall-prefix=,(common-lisp-src)clisp/tools/x86_64-unknown-linux-gnu)
             --with-readline --with-libreadline-prefix=/usr
             --with-sigsegv --with-libsigsegv-prefix=/usr
             --with-module=asdf
             --with-module=bindings/glibc
             --with-module=clx/new-clx
             --with-module=dbus
             --with-module=i18n
             --with-module=rawsock
             --with-module=readline
             --with-module=regexp
             --with-module=syscalls
             --with-module=zlib
             ;;--with-module=berkeley-db
             ;;--with-module=dirkey
             ;;--with-module=gdbm
             ;;--with-module=gtk2
             ;;--with-module=fastcgi
             ;;--with-module=libsvm
             ;;--with-module=matlab
             ;;--with-module=netica
             ;;--with-module=oracle
             ;;--with-module=netica
	     ;;--with-module=pari
             ;;--with-module=pcre
             ;;--with-module=postgresql
             ;;--with-module=queens
             --cbc build-dir
             (--prefix=,(stow-root)clisp)))
    ;;(run/i `(make "-C" build-dir distclean)
    (run/i `(make "-C" build-dir))
    (run/i `(make "-C" build-dir check))
    (run/i `(make "-C" build-dir install (prefix=,(stow-root)clisp))))
  (success))

(defun myecl ()
  (with-current-directory ((subpathname (common-lisp-src) "ecl/"))
    (run/i `(git clean -xfd))
    (run/i `(./configure (--prefix=,(stow-root)ecl)))
    (run/i `(make -l6))
    (run/i `(make install))))

(defun mymkcl ()
  (with-current-directory ((subpathname (common-lisp-src) "mkcl/"))
    (run/i `(git clean -xfd))
    (run/i `(./configure (--prefix=,(stow-root)mkcl)))
    (run/i `(make -l6))
    (run/i `(make install))))

(defun mygcl ()
  ;; git clone git://git.sv.gnu.org/gcl.git
  (with-current-directory ((subpathname (common-lisp-src) "gcl/gcl/"))
    (run/i `(git clean -xfd))
    (run/i `(./configure --enable-ansi (--prefix=,(stow-root)gcl))) ;; --disable-dynsysgmp --enable-static ???
    (run/i `(make -l6 install (prefix=,(stow-root)gcl)))
    (delete-file (subpathname (stow-root) "gcl/share/info/dir"))
    (success)))

(defun mysbcl ()
  (with-current-directory ((subpathname (common-lisp-src) "sbcl/"))
    (let ((install-root (subpathname (stow-root) "sbcl/"))
          (out (subpathname (temporary-directory) "mysbcl.out")))
      (run/i `(pipe ("sh" "./make.sh" ("--prefix=",(nns install-root))
                          "--xc-host=/usr/bin/sbcl --disable-debugger --no-userinit --no-sysinit"
                          "--with-sb-dynamic-core" "--fancy" (>& 2 1)) (tee ,out)))
      (ignore-errors
       (delete-directory-tree (subpathname install-root "lib/sbcl/") :validate #'(lambda (p) (subpathp p install-root))))
      (run/i `(pipe (sh "./install.sh" (--prefix=,(nns install-root))) (tee -a ,out)))))
  (success))

(defun mysbcl-contrib (&optional tag)
  (let ((install-root (subpathname (stow-root) "sbcl/"))
        (out (subpathname (temporary-directory) "mysbcl-contrib.out")))
    (with-current-directory ((subpathname (common-lisp-src) "sbcl/obj/asdf-upstream/"))
      (run/i `(pipe (git reset --hard ,tag) (tee ,out)))
      (run/i `(pipe (git pull (subpathname (cl-root) "asdf/")) (tee -a ,out))))
    (with-current-directory ((subpathname (common-lisp-src) "sbcl/contrib/asdf/"))
      (run/i `(pipe (make up "SBCL=../../run-sbcl.sh") (tee -a ,out))))
    (with-current-directory ((subpathname (common-lisp-src) "sbcl/"))
      (run/i `(pipe (sh "./make-target-contrib.sh" (--prefix=,install-root) --with-sb-core-compression)
                    (tee -a out)))
      (run/i `(pipe (sh "./install.sh" (--prefix=,install-root)) (tee -a out)))))
  (success))

(defun myrust ()
  (with-current-directory ((subpathname (src-root) "proglang/rust/"))
    (let ((install-root (subpathname (stow-root) "rust/"))
          (out (subpathname (temporary-directory) "myrust.out")))
      (run/i `(pipe (sh "./configure" (--prefix=,install-root)) (tee ,out)))
      ;;(delete-directory-tree (subpathname install-root "lib/rust/") :validate #'(lambda (p) (subpathp p install-root)))
      (run/i `(pipe (make install) (tee -a ,out)))))
  (success))

(defun myplt ()
  (with-current-directory ((subpathname (src-root) "racket/plt/"))
    (run/i `(git clean -xfd))
    (run/i `(pipe (make -l6 unix-style ("PREFIX"=,(stow-root)plt) (>& 2 1))
                  (tee /tmp/plt.out))))
  (success))

(defun frob ()
  (with-current-directory ((subpathname (src-root) "fare/ngnghm/"))
    (run/i '(raco frog -b)))
  (success))

(defun frop ()
  (with-current-directory ((subpathname (src-root) "fare/ngnghm/"))
    (run/i '(raco frog -bp)))
  (success))

(defun myccl ()
  (with-current-directory ((subpathname (common-lisp-src) "ccl/"))
    (loop :repeat 2 :do
      (run/i '("./lx86cl64" "--no-init" "--eval" "(progn (ccl:rebuild-ccl :full t) (ccl:quit 0))"))))
  (success))

(defun upccl (version)
  (with-current-directory ((subpathname (common-lisp-src) "ccl/"))
    (loop
      :for external :in (cons " ." (run/lines '(svn propget "svn:externals" ".")))
      :for pos = (position #\space external)
      :for dir = (when pos (subseq external (1+ pos)))
      :when dir
      :do (run/i `(svn up (-r ,version) --ignore-externals ,dir) :show t))
    (success)))

);exporting-definitions

(register-commands :fare-scripts/languages)
