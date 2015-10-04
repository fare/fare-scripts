;;; REPL utilities
(uiop:define-package :fare-scripts/repl
  (:use :cl :fare-utils :uiop :inferior-shell :optima :optima.ppcre :cl-scripting))

(in-package :fare-scripts/repl)

;; From http://lispblog.xach.com/post/129215925278/my-new-favorite-slimesbclccl-trick
#+sbcl
(push (lambda (&rest args) (apply #'swank:ed-in-emacs args) t) sb-ext:*ed-functions*)
#+ccl
(setq ccl:*resident-editor-hook* #'swank:ed-in-emacs)
