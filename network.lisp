;;; REPL utilities
(uiop:define-package :fare-scripts/network
  (:use :cl :fare-utils :uiop :inferior-shell :optima :optima.ppcre :cl-scripting)
  (:export
   #:get-wireless-passphrase
   #:nmup #:nmauto))

(in-package :fare-scripts/network)

;;; The WIRELESS_SECRETS environment variable should point to a file where
;;; some lines of the form "ESSID: your ssid" are followed by
;;; a line "passphrase: your passphrase", in order of network preference.
;;;
(defvar *wireless-secrets* nil)
(defun init-wireless-secrets ()
  (setf *wireless-secrets* (getenv-pathname "WIRELESS_SECRETS")))
(register-image-restore-hook 'init-wireless-secrets t)

(defun get-wireless-secrets ()
  (or *wireless-secrets* (error "WIRELESS_SECRETS variable not defined")))

(defun extract-fields (field-lengths line)
  (loop :for (name start end) :in field-lengths :collect
    (progn
      name ;; ignore
      (string-right-trim " " (subseq line start end)))))

(defun extract-field-lengths (fields)
  (loop :with start = 0 :with len = (length fields)
    :while (< start len) :collect
    (let* ((name-end (position #\space fields :start start))
           (name (subseq fields start name-end))
           (end (or (when name-end (position #\space fields :start name-end :test-not #'eql)) len)))
      (prog1
          (list name start end)
        (setf start end)))))

(defun parse-nmcli-list-line (field-lengths)
  (lambda (line)
    (destructuring-bind (connected ssid mode chan rate signal bars security)
        (extract-fields field-lengths line)
      (declare (ignore bars))
      (list (equal connected "*")
            ssid
            mode
            (parse-integer chan)
            (parse-integer rate :junk-allowed t) ;; "54 Mbit/s"
            (parse-integer signal)
            (split-string (string-right-trim " " security))))))

(defun nmcli-list ()
  (destructuring-bind (fields . lines) (run/lines '(nmcli device wifi list))
    (let ((field-lengths (extract-field-lengths fields)))
      (values (mapcar (parse-nmcli-list-line field-lengths) lines)
              field-lengths))))

(exporting-definitions

(defun get-wireless-passphrase (essid)
  (with-input-file (s (get-wireless-secrets))
    (loop :with expected = (strcat "ESSID: " essid)
      :for line = (read-line s nil nil) :while line :do
      (if (equal line expected)
          (match (read-line s nil nil)
            ((ppcre "^pass(?:word|phrase): (.*)$" pass) (return pass)))))))

(defun nmup (&optional connection passphrase)
  (if connection
      (let ((passphrase (or passphrase (get-wireless-passphrase connection))))
        (with-temporary-file (:stream s :pathname passwd-file)
          (format s "802-11-wireless-security.psk:~a~%" passphrase)
          :close-stream
          (run/i `(nmcli connection up ,connection passwd-file ,passwd-file))
          (success)))
      (nmauto)))

(defun nmauto ()
  (loop :with table = (make-hash-table :test 'equal)
    :for network :in (nmcli-list)
    :for ssid = (second network)
    :do (setf (gethash ssid table) t)
    :finally
    (with-input-file (s (get-wireless-secrets))
      (loop :for line = (read-line s nil nil) :while line :do
        (when (string-prefix-p "ESSID: " line)
          (let ((ssid (subseq line #.(length "ESSID: "))))
            (when (gethash ssid table)
              (match (read-line s nil nil)
                ((ppcre "^pass(?:word|phrase): (.*)$" pass)
                 (nmup ssid pass)
                 (return-from nmauto ssid))))))))))
)

(register-commands :fare-scripts/network)
