;; Shell aliases for working with bazel...

(uiop:define-package :fare-scripts/bazel
  (:use :cl :fare-utils :uiop :inferior-shell :optima :optima.ppcre :cl-scripting))

(in-package :fare-scripts/bazel)

(exporting-definitions

(defparameter *bazel-dir* (subpathname (user-homedir-pathname) "src/google/bazel/"))
(defparameter *bazel* (subpathname *bazel-dir* "output/bazel"))

(defun bazel (&rest args)
  (with-current-directory (*bazel-dir*)
    (run/i `(,*bazel* ,@args)))
  (success))

(defun ss (&optional nobuild)
  (with-current-directory (*bazel-dir*)
    (unless nobuild
      (bazel 'build "src/test/java:skylarkshell"))
    (run/i '("bazel-bin/src/test/java/skylarkshell")))
  (success))

);exporting-definitions

(register-commands :fare-scripts/bazel)
