":" ; exec cl-launch -Q -sm fare-scripts/typographie -- "$@"

(defpackage :fare-scripts/typographie
  (:use :common-lisp :uiop :cl-ppcre)
  (:export #:typographie #:ligne))

(in-package :fare-scripts/typographie)

(defparameter *spaces*
  ;; space, no-break-space, thin space, narrow no-break space, tab
  (map 'string 'code-char '(#x20 #xA0 #x2009 #x202F #x9)))

(defun space-char (&key thin unbreakable)
  (aref *spaces* (+ (if unbreakable 1 0) (if thin 2 0))))

(defparameter *any-space* `(:regex ,(strcat "[" *spaces* "]")))
(defparameter *pre-demi-espace*
  (create-scanner
   `(:sequence
     (:greedy-repetition 0 nil ,*any-space*)
     (:regex "([;!?])")
     (:greedy-repetition 0 nil ,*any-space*))))
(defparameter *pre-espace*
  (create-scanner
   `(:sequence
     (:greedy-repetition 0 nil ,*any-space*)
     (:regex "([:»])")
     (:negative-lookahead "[/,.:;!?]")
     (:greedy-repetition 0 nil ,*any-space*))))
(defparameter *post-espace*
  (create-scanner
   `(:sequence
     (:regex "(«)")
     (:greedy-repetition 0 nil ,*any-space*))))
(defparameter *pas-d-espace*
  (create-scanner
   `(:alternation
     (:sequence
      (:greedy-repetition 0 nil ,*any-space*)
      (:positive-lookahead (:regex "$|[)\\]}]")))
     (:sequence
      (:positive-lookahead (:regex "$|[(\\[{]"))
      (:greedy-repetition 0 nil ,*any-space*)))))

(defun ligne (s)
  (let* ((s1 (regex-replace-all *pre-demi-espace* s
                                (strcat (space-char :thin t :unbreakable t) "\\1 ")))
         (s2 (regex-replace-all *pre-espace* s1
                                (strcat (space-char :unbreakable t) "\\1 ")))
         (s3 (regex-replace-all *post-espace* s2
                                (strcat "\\1" (space-char :unbreakable t)))))
    (regex-replace-all *pas-d-espace* s3 "")))

(defun typographie (&optional (i *standard-input*) (o *standard-output*))
  (loop for r = (read-line i nil nil)
        while r do
          (format o "~A~%" (ligne r))))

(defun main (argv)
  (assert (null argv))
  (typographie))
