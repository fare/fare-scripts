;; Slowly migrating my zsh aliases here...

(uiop:define-package :fare-scripts/shell-aliases
  (:use :cl :fare-utils :uiop
   :inferior-shell :cl-scripting/failure :cl-scripting/commands :fare-scripts/commands
   :optima :optima.ppcre
   :cl-launch/dispatch)
  #+sbcl (:import-from :sb-posix))

(in-package :fare-scripts/shell-aliases)

(exporting-definitions

(defun mkba ()
  (with-current-directory ((subpathname (user-homedir-pathname) "src/fare/bastiat.org/"))
    (run '(make dep)) (run '(make)))
  (success))

(defun mkba2 ()
  (mkba) (mkba))

(defun mygcl ()
  ;; git clone git://git.sv.gnu.org/gcl.git
  (with-current-directory ((subpathname (common-lisp-src) "gcl/gcl/"))
    (run `(git clean -xfd))
    (run `(./configure --enable-ansi (--prefix=,(stow-root)gcl))) ;; --disable-dynsysgmp --enable-static ???
    (run `(make -l6 install (prefix=,(stow-root)gcl)))
    (delete-file (subpathname (stow-root) "gcl/share/info/dir"))
    (success)))

(defun char-display-char (c)
  (if (or (member c '(127 155))
	  (< c 32)
	  (<= 128 c 159))
      #\space
    (code-char c)))

(defvar *num-mode* "[31m")
(defvar *colon-mode* "[34m")
(defvar *char-mode* "[0m[1m")
(defvar *normal-mode* "[0m")

(defun display-ascii-hex-table ()
  (loop for i from 32 to 255
    do (format t "~A~X~A:~A~A~A~:[ ~;~%~]"
	       *num-mode* i
	       *colon-mode* *char-mode*
	       (char-display-char i)
	       *normal-mode*
               (zerop (mod (1+ i) 16))))
  (success))

(defun ascii () (display-ascii-hex-table))

(defun display-ascii-oct-table ()
  (loop for i from 32 to 255
    do (format t "~A~3O~A~A~A~:[ ~;~%~]"
	       *num-mode* i
	       *char-mode*
	       (char-display-char i)
	       *normal-mode*
               (zerop (mod (1+ i) 16))))
  (success))

(defun rot13 ()
  (run/interactive '(tr "[a-zA-Z]" "[n-za-mN-ZA-M]"))
  (success))

(defun xrsync (args)
  (run `(rsync "-rlptgoDHSx" ,@args)))

(defun snd-jack ()
  (run `(pulseaudio --kill))
  (run `(jack_control start)))

(defun snd-pulse ()
  (run `(jack_control exit))
  (run `(pulseaudio --start)))

(defun snd-nojack ()
  (run `(killall jackd)))
);exporting-definitions

(register-commands :fare-scripts/shell-aliases)
