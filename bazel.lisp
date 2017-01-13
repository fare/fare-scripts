;; Shell aliases for working with bazel...

(uiop:define-package :fare-scripts/bazel
  (:use :cl :fare-utils :uiop :inferior-shell :optima :optima.ppcre :cl-scripting)
  (:export #:*bazel-dir* #:*bazel* #:bazel #:ss #:ngr #:make-bazel
           #:*java-home*))

(in-package :fare-scripts/bazel)

(exporting-definitions

(defparameter *java-home* "/usr/lib/jvm/java-8-openjdk-amd64")
(defparameter *bazel-dir* (subpathname (user-homedir-pathname) "src/google/bazel/"))
(defparameter *bazel* (subpathname *bazel-dir* "output/bazel"))

(defun make-bazel ()
  (with-current-directory (*bazel-dir*)
    (run/i `(env ("JAVA_HOME=" ,*java-home*) "./compile.sh"))
    (values)))

(defun bazel (&rest args)
  (with-current-directory (*bazel-dir*)
    (run/i `(env ("JAVA_HOME=" ,*java-home*) ,*bazel* ,@args)))
  (success))

(defun ss (&optional nobuild)
  (with-current-directory (*bazel-dir*)
    (unless nobuild
      (bazel 'build "src/test/java:skylarkshell"))
    (run/i '("bazel-bin/src/test/java/skylarkshell")))
  (success))


(progn
  (defun directory-last-name (path)
    (let ((n (last (pathname-directory path))))
      (and (consp n) (stringp (car n)) (car n))))
  (defun repo-char-p (char)
    (ascii-alphanumeric-or-underscore-p char)))

(defun ngr (&optional path)
  (with-current-directory ((and path (ensure-directory-pathname path)))
    (let* ((url
	    (match (run/ss '(git config --get remote.origin.url))
	      ((ppcre "^(ssh:|git:)?(//)?(git@)?(github.com|gitlab.common-lisp.net)[:/](.*)$"
		      _ _ _ h p)
	       (strcat "https://" h "/" p))
	      (url url)))
	   (commit (run/ss '(git log -1 "--format=%H")))
	   (dirname (directory-last-name (getcwd)))
	   (repo (strcat "lisp__" (substitute-if-not #\_ #'repo-char-p
						     (string-downcase dirname)))))
      (format t "  native.new_git_repository(
      name = \"~A\",
      commit = \"~A\",
      remote = \"~A\",
      build_file = base_dir + \"/build_defs/~A.BUILD\"
  )~%~%"
	      repo commit url repo)
      (values))))
);exporting-definitions

(register-commands :fare-scripts/bazel)
