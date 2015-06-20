;; Shell aliases for working with bazel...

(uiop:define-package :fare-scripts/bazel
  (:use :cl :fare-utils :uiop :inferior-shell :optima :optima.ppcre :cl-launch/dispatch
   :cl-scripting/failure)
  #+sbcl (:import-from :sb-posix))

(in-package :fare-scripts/bazel)

(exporting-definitions

(defparameter *bazel-dir* (subpathname (user-homedir-pathname) "src/google/bazel/"))
(defparameter *bazel* (subpathname *bazel-dir* "output/bazel"))

(defun bazel (&rest args)
  (with-current-directory (*bazel-dir*)
    (run/interactive `(,*bazel* ,@args)))
  (success))

(defun ss (&optional nobuild)
  (with-current-directory (*bazel-dir*)
    (unless nobuild
      (bazel 'build "src/test/java:skylarkshell"))
    (run/interactive '("bazel-bin/src/test/java/skylarkshell")))
  (success))

);exporting-definitions

(do-external-symbols (cmd :fare-scripts/bazel)
  (when (fboundp cmd)
    (cl-launch/dispatch:register-entry (string-downcase cmd)
       (lambda (argv) (apply 'run-command cmd argv)))))
