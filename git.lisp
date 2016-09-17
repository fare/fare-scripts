;; Personal scripts to deal with git

(uiop:define-package :fare-scripts/git
  (:use :cl :fare-utils :uiop
   :inferior-shell :cl-scripting :fare-scripts/commands
   :optima :optima.ppcre :cl-ppcre
   :cl-launch/dispatch)
  (:export
   #:gb))

(in-package :fare-scripts/git)

(exporting-definitions

(defun gb (&optional pattern)
  (if pattern
      (let* ((branches* (run/lines `(git branch)))
             (branches (mapcar (lambda (s) (subseq s 2)) branches*))
             (matches (loop :for b :in branches :when (scan pattern b) :collect b)))
        (case (length matches)
          (0 (fail! "No branch matches ~S. Branches available:~%~{~A~%~}" pattern branches*))
          (1 (let ((branch (first matches)))
               (run/i `(git checkout ,branch))
               (success)))
          (otherwise
           (fail! "Several branches match ~S:~%~{  ~A~%~}" pattern matches))))
      (progn
        (run/i `(git branch))
        (success))))

);exporting-definitions

(register-commands :fare-scripts/git)
