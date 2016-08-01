":" ; exec cl-launch -Q -sm fare-scripts/unmime -- "$@"

(uiop:define-package :fare-scripts/unmime
  (:mix :cl :cl-mime :uiop :optima :optima.ppcre :babel))

(in-package :fare-scripts/unmime)

(defun show-usage (&optional (stream *standard-output*))
  (format stream "usage: unmime | unmime - | unmime <filename>~%~
	decodes the specified file (default: stdin) as a single-file mime container~%"))

(defun get-parsed-mime (x)
  (etypecase x
    ((or stream string) (cl-mime:parse-mime x))
    (pathname (with-input-file (s x) (cl-mime:parse-mime s)))))

(defun decode-text-mime (tm)
  (let* ((c (coerce (decode-content tm) '(vector (unsigned-byte 8) *)))
	 (cs (first (charset tm)))
	 (e (babel-encodings:get-character-encoding
	     (and cs (find-symbol (string-upcase cs) :keyword)))))
    (octets-to-string c :encoding e)))

(defun get-mime-string (x)
  (let* ((m (get-parsed-mime x))
         (tm (etypecase m
               (text-mime m)
               (multipart-mime (let ((f (first (content m))))
                                 (check-type f text-mime)
                                 f)))))
    (decode-text-mime tm)))

(defun unmime (x) (princ (get-mime-string x)))

(defun unmimeall (input output-name)
  (let ((m (get-parsed-mime input)))
    (unmime-to m output-name)))

(defun unmime-to (m output-name)
  (etypecase m
    (text-mime
     (format! t "Creating text file ~A~%" output-name)
     (with-output-file (o output-name :if-exists :rename-and-delete)
       (princ (decode-text-mime m) o)))
    (multipart-mime
     (loop
       :for i :from 0
       :for n :in (content m)
       :do (unmime-to n (format nil "~A.~D" output-name i))))
    (mime
     (format! t "Creating binary file ~A~%" output-name)
     (with-output-file (o output-name :element-type '(unsigned-byte 8) :if-exists :rename-and-delete)
       (write-sequence (decode-content m) o)))))

(defun main (argv)
  (match argv
    (() (unmime *standard-input*))
    ((list x)
     (match x
       ("-" (unmime *standard-input*))
       ((or "-h" "--help" "-?") (show-usage *standard-output*))
       ((ppcre "^-") (show-usage *error-output*))
       (_ (unmime (parse-native-namestring x)))))
    ((list in out)
     (unmimeall
      (if (equal in "-") *standard-input* (parse-native-namestring in))
      out))
    (_ (show-usage *error-output*))))
