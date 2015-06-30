":" ; exec cl-launch -Q -sm fare-scripts/unmime -- "$@"

(defpackage :fare-scripts/unmime
  (:use :cl :cl-mime :uiop :optima :optima.ppcre :babel))

(in-package :fare-scripts/unmime)

(defun show-usage (&optional (stream *standard-output*))
  (format stream "usage: unmime | unmime - | unmime <filename>~%~
	decodes the specified file (default: stdin) as a single-file mime container~%"))

(defun get-mime-string (x)
  (let* ((m (etypecase x
              ((or stream string) (cl-mime:parse-mime x))
              (pathname (with-input-file (s x) (cl-mime:parse-mime s)))))
         (tm (etypecase m
               (text-mime m)
               (multipart-mime (let ((f (first (content m))))
                                 (check-type f text-mime)
                                 f))))
         (c (coerce (decode-content tm) '(vector (unsigned-byte 8) *)))
         (cs (first (charset tm)))
         (e (babel-encodings:get-character-encoding
             (and cs (find-symbol (string-upcase cs) :keyword)))))
    (octets-to-string c :encoding e)))

(defun unmime (x) (princ (get-mime-string x)))

(defun main (argv)
  (match argv
    (() (unmime *standard-input*))
    ((list x)
     (match x
       ("-" (unmime *standard-input*))
       ((or "-h" "--help" "-?") (show-usage *standard-output*))
       ((ppcre "^-") (show-usage *error-output*))
       (_ (unmime (parse-native-namestring x)))))
    (_ (show-usage *error-output*))))
