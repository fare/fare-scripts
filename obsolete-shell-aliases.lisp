;; Slowly migrating my zsh aliases here...

(uiop:define-package :fare-scripts/obsolete-shell-aliases
  (:use :cl :fare-utils :uiop :inferior-shell :optima :optima.ppcre :cl-launch/dispatch
   :asdf-tools) ;; for its run-command, success, failure
  #+sbcl (:import-from :sb-posix))

(in-package :fare-scripts/obsolete-shell-aliases)

(def*fun tcdr (&rest args) (run `(cdrecord -v dev=4,0,0 speed=32 ,@args)))
(def*fun tdao (cmd &rest args) (run `(cdrdao ,cmd --device 4,0,0 --driver generic-mmc --speed 32 ,@args)))

(do-external-symbols (cmd :fare-scripts/obsolete-shell-aliases)
  (cl-launch/dispatch:register-entry (string-downcase cmd)
     (lambda (argv) (apply 'run-command cmd argv))))
