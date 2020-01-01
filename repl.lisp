;;; REPL utilities
(uiop:define-package :fare-scripts/repl
  (:use :cl :fare-utils :uiop :inferior-shell :optima :optima.ppcre :cl-scripting :cl-launch/dispatch)
  (:import-from :swank)
  (:export #:read-eval-print #:rep))

(in-package :fare-scripts/repl)

;; From http://lispblog.xach.com/post/129215925278/my-new-favorite-slimesbclccl-trick
#+sbcl
(push (lambda (&rest args) (apply #'swank:ed-in-emacs args) t) sb-ext:*ed-functions*)
#+ccl
(setq ccl:*resident-editor-hook* #'swank:ed-in-emacs)

(exporting-definitions
(defun read-eval-print (s &optional (package :fare-scripts/repl))
  (with-standard-io-syntax
    (let ((*package* (find-package (standard-case-symbol-name package))))
      (format t "~W~%" (eval-input s)))))

(defun rep (a &optional b)
  (if b (read-eval-print b a) (read-eval-print a))))
(register-commands :fare-scripts/repl)

(eval-when (:compile-toplevel)
  (uiop:println "COMPILE-TOPLEVEL"))
(eval-when (:load-toplevel)
  (uiop:println "LOAD-TOPLEVEL"))
(eval-when (:execute)
  (uiop:println "EXECUTE"))
