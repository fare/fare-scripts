;;; REPL utilities
(uiop:define-package :fare-scripts/network
  (:use :cl :fare-utils :uiop :inferior-shell :optima :optima.ppcre :cl-scripting)
  (:export
   #:get-wireless-passphrase
   #:nmup))

(in-package :fare-scripts/network)

(defvar *wireless-secrets* nil)
(defun init-wireless-secrets ()
  (setf *wireless-secrets* (getenv-pathname "WIRELESS_SECRETS")))
(register-image-restore-hook 'init-wireless-secrets t)

(exporting-definitions

(defun get-wireless-passphrase (essid)
  (with-input-file (s *wireless-secrets*)
    (loop :with expected = (strcat "ESSID: " essid)
      :for line = (read-line s nil nil) :while line :do
      (if (equal line expected)
          (match (read-line s nil nil)
            ((ppcre "^pass(?:word|phrase): (.*)$" pass) (return pass)))))))

(defun nmup (connection)
  (nest
   (let ((passphrase (get-wireless-passphrase connection))))
   (with-temporary-file (:stream s :pathname passwd-file)
     (format s "802-11-wireless-security.psk:~a~%" passphrase)
     :close-stream)
   (run/i `(nmcli connection up ,connection passwd-file ,passwd-file)))
  (success))

)

(register-commands :fare-scripts/network)
