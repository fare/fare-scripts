;; Personal scripts to deal with various programming languages

(uiop:define-package :fare-scripts/languages
  (:use :cl :fare-utils :uiop
   :inferior-shell :cl-scripting :fare-scripts/commands
   :optima :optima.ppcre
   :cl-launch/dispatch)
  (:export
   #:frob #:frop #:mkba #:mkba2
   #:myccl #:mychez #:myclisp #:myecl #:mygcl #:myhott #:mymkcl #:myplt
   #:myrust #:mysbcl #:mysbcl-contrib #:upccl #:mygambit #:mygerbil))

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

(defun myclisp (&optional debug)
  (with-current-directory ((subpathname (common-lisp-src) "clisp/"))
    (run/i `(hg clean)) ;; requires "[extensions]\npurge = " in ~/.hgrc
    (ignore-errors
      (delete-directory-tree
       (subpathname (common-lisp-src) "clisp/build-dir/")
       :validate (lambda (p)
		   (equal "build-dir" (car (last (pathname-directory p)))))))
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
             (--prefix=,(stow-root)clisp)
	     --cbc
	     ;; The below line is for debugging the GC. See CLISP bug 678
	     ;;   https://sourceforge.net/p/clisp/bugs/678/
	     ,@(when debug '(--with-debug "CC=g++"))
             build-dir))
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

(defun mysbcl (&optional (install-root (subpathname (stow-root) "sbcl/")))
  (DBG "Compiling a custom SBCL")
  (with-current-directory ((subpathname (common-lisp-src) "sbcl/"))
    (let ((out (subpathname (temporary-directory) "mysbcl.out")))
      (run/i `(pipe ("sh" "./make.sh" ("--prefix=",(nns install-root))
                          ,@(when (probe-file "/usr/bin/sbcl")
                              '("--xc-host=/usr/bin/sbcl --disable-debugger --no-userinit --no-sysinit"))
                          "--with-sb-threads" "--with-sb-linkable-runtime" "--with-sb-dynamic-core"
                          "--fancy" (>& 2 1))
                    (tee ,out)) :show t)
      (ignore-errors
        (delete-directory-tree (subpathname install-root "lib/sbcl/")
                               :validate #'(lambda (p) (subpathp p install-root))))
      (run/i `(pipe (sh "./install.sh" (--prefix=,(nns install-root)))
                    (tee -a ,out)) :show t)))
  (success))

(defun mysbcl-contrib (&optional (install-root (subpathname (stow-root) "sbcl/")))
  (let ((out (subpathname (temporary-directory) "mysbcl-contrib.out")))
    (with-current-directory ((subpathname (common-lisp-src) "sbcl/obj/asdf-upstream/"))
      (run/i `(pipe (git pull (subpathname (cl-root) "asdf/")) (tee ,out))))
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

(defun mychez ()
  (with-current-directory ((subpathname (src-root) "scheme/ChezScheme/"))
    (run/i `(git clean -xfd))
    (run/i `("./configure"
	     "--threads"
	     ("--installprefix=",(stow-root)"ChezScheme")
	     ("--installman=",(stow-root)"share/man")))
    (run/i `(pipe ("make" "-l6" "-C" "ta6le" "install" (>& 2 1))
                  (tee /tmp/chez.out))))
  (success))

(defun myhott ()
  (with-current-directory ((subpathname (src-root) "coq/HoTT/"))
    (run/i `(etc/install_coq.sh))
    (run/i `(./autogen.sh))
    (run/i `(./configure ("COQBIN=",(getcwd)"/coq-HoTT/bin")))
    (run/i `(make))))

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

(defun mygambit ()
  (with-current-directory ((subpathname (src-root) "scheme/gambit/"))
    ;; This assumes that https://github.com/gambit/gambit/pull/279 is merged in.
    (run/i `(git clean -xfd))
    (run/i `("./configure"
             ;; https://github.com/vyzo/gerbil/wiki/Getting-Started-with-Gerbil-development
             ("--prefix=" ,(stow-root) "gambit")
             "--enable-targets=js" ;; "arm,java,js,php,python,ruby,x86,x86-64"
             "--enable-single-host"
             "--enable-c-opt=-O1" ;; -O1 compiles faster, even though -Os has overall better performance
             "--enable-c-opt-rts=-O2"
             "--enable-gcc-opts"
             "--enable-shared"
             "--enable-absolute-shared-libs"
             "--enable-poll"
             "--enable-openssl"
             "--enable-trust-c-tco"
             "--enable-dynamic-clib"
             ;;"--enable-default-runtime-options=f8,-8,t8" ;; Default to UTF-8 for source and all I/O
             ;; "--enable-guide"
             ;; "--enable-profile"
             ;; "--enable-coverage"
             ;; "--enable-inline-jumps"
             ;; "--enable-char-size=1" ; default is 4
             ;; "--enable-multiple-versions"
             ;; "--enable-multiple-vms"
             ;; "--enable-smp"
             ;; "--enable-thread-system"
             ;; "--enable-max-processors=4"
             ;; "--enable-track-scheme"
             ;; "--enable-high-res-timing"
             ;; "--enable-thread-system=posix"
             ;; "--enable-dynamic-tls"
             ;; "--enable-openssl"
             ))
    (run/i `("make" "-j4" "current-gsc-boot"))
    (run/i `("make" "-j4" "from-scratch"))
    (run/i `("make" "check"))
    (run/i `("make" "-j4" "modules"))
    (run/i `("make" "install")))
  (success))

(defun mygerbil ()
  (with-current-directory ((subpathname (src-root) "fare/gerbil"))
    ;; TODO: export GERBIL_BUILD_CORES ?
    (run/i `("./configure" ("--prefix=" ,(stow-root) "gerbil")
                           ;;("--with-gambit=" ,(stow-root) "gambit/") ;; now builtin
                           "--enable-shared"
                           "--enable-deprecated"
                           "--enable-libxml"
                           "--enable-libyaml"
                           "--enable-zlib"
                           "--enable-sqlite"
                           "--enable-mysql"
                           "--enable-lmdb"
                           "--enable-leveldb"))
    (run/i `("./build.sh"))
    (run/i `("./install.sh"))))

);exporting-definitions

(register-commands :fare-scripts/languages)
