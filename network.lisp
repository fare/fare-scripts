;;; REPL utilities
(uiop:define-package :fare-scripts/network
  (:use :cl :fare-utils :uiop :inferior-shell :optima :optima.ppcre :cl-scripting)
  (:export
   #:wireless-connection-status
   #:wireless-devices #:wireless-device-status
   #:get-network-connections #:get-wireless-passphrase
   #:get-wireless-devices
   #:nmup #:nmauto #:nowifi))

(in-package :fare-scripts/network)

;;; The WIRELESS_SECRETS environment variable should point to a file where
;;; some lines of the form "ESSID: your ssid" are followed by
;;; a line "passphrase: your passphrase", in order of network preference.
;;;
;;; Q: should it default to ~/.secrets/wireless.text ???
(defvar *wireless-secrets* nil)
(defun init-wireless-secrets ()
  (setf *wireless-secrets* (getenv-pathname "WIRELESS_SECRETS")))


(register-image-restore-hook 'init-wireless-secrets t)

(defun bars-string (level noise)
  (let* ((snr (- level noise))) ;; signal-to-noise ratio
    (cond ;; ▂▄▆█ ▂▄▆░ ▂▄░░ ▂░░░ ░░░░ ;; ▂▄▆█ ▂▄▆_ ▂▄__ ▂___ ____
      ((>= snr 40) "****")
      ((>= snr 25) "***_")
      ((>= snr 15) "**__")
      ((>= snr  5) "*___")
      (t           "____"))))

(defun wpa-cli (&rest arguments)
  (mapcar #'parse-environment-line (run/lines (list* "sudo" "wpa_cli" arguments))))

(defun parse-environment-line (line)
  (let ((p (position #\= line)))
    (if p
        (cons (subseq line 0 p)
              (subseq line (1+ p) (length line)))
        (cons t line))))

(defun get-wireless-secrets ()
  (or *wireless-secrets* (error "WIRELESS_SECRETS variable not defined")))

(defun extract-fields (field-lengths line)
  (loop :with len = (length line)
    :for (name start end) :in field-lengths :collect
    (progn
      name ;; ignore
      (string-right-trim " " (subseq line start (min end len))))))

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
    (destructuring-bind (in-use bssid ssid mode chan rate signal bars security)
        (extract-fields field-lengths line)
      (list (equal in-use "*")
            bssid
            ssid
            mode
            (parse-integer chan)
            (parse-integer rate :junk-allowed t) ;; "54 Mbit/s"
            (parse-integer signal)
            bars
            (split-string (string-right-trim " " security))))))

;; in-use bssid ssid mode chan rate signal bars security
(defun nmcli-list (&optional ifname)
  (destructuring-bind (fields . lines)
      (run/lines `(nmcli device wifi list --rescan no ,@(when ifname `(ifname ,ifname))))
    (let ((field-lengths (extract-field-lengths fields)))
      (values (mapcar (parse-nmcli-list-line field-lengths) lines)
              field-lengths))))

(exporting-definitions

(defun wireless-devices ()
  (loop :for p :in (directory "/sys/class/net/*/wireless")
        :collect (cadr (reverse (pathname-directory p)))))

(defun wireless-device-status (device)
  (labels ((get-string (env name)
             (cdr (assoc name env :test 'equal)))
           (get-integer (env name)
             (ignore-errors (parse-integer (get-string env name)))))
    (let* ((status (wpa-cli "-i" device "status"))
           (ssid (get-string status "ssid"))
           (bssid (get-string status "bssid"))
;           (_ (DBG :foo status ssid bssid))
           (bss (wpa-cli "bss" bssid))
           (level (get-integer bss "level"))
           (noise (get-integer bss "noise")))
      (list device ssid (bars-string level noise)))))

(defun wireless-connection-status (&optional (s t))
  "Wireless connection status"
  (loop :for (device ssid bars) :in (mapcar #'wireless-device-status (wireless-devices))
        :when ssid
          :do (format s "~A connected to ~A ~A~%" device ssid bars)))

;; (list-of (list name uuid type device)) <-
(defun get-network-connections ()
  (destructuring-bind (fields . lines) (run/lines '(nmcli connection show --active))
    (let ((field-lengths (extract-field-lengths fields)))
      (loop :for line :in lines :collect (extract-fields field-lengths line)))))

(defun get-wireless-passphrase (essid)
  (with-input-file (s (get-wireless-secrets))
    (loop :with expected = (strcat "ESSID: " essid)
      :for line = (read-line s nil nil) :while line :do
      (if (equal line expected)
          (match (read-line s nil nil)
            ((ppcre "^pass(?:word|phrase): (.*)$" pass) (return pass)))))))

(defun nmup (&optional connection (passphrase :auto))
  (if connection
      (let ((passphrase
             (if (eq passphrase :auto)
                 (get-wireless-passphrase connection)
                 passphrase)))
        (if passphrase
            (with-temporary-file (:stream s :pathname passwd-file)
              (format s "802-11-wireless-security.psk:~a~%" passphrase)
              :close-stream
              (run/i `(nmcli connection up ,connection passwd-file ,passwd-file (>& 1 2))))
            (run/i `(nmcli --ask connection up ,connection)))
        (success))
      (nmauto)))

(defun nowifi ()
  (dolist (connection (get-network-connections))
    (destructuring-bind (name uuid type device) connection
      (declare (ignore uuid device))
      (when (equal type "wifi")
        (run/i `(nmcli connection down ,name))))))

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
                 (nmup ssid pass))
                (_ (nmup ssid nil)))
              (return-from nmauto ssid))))))))
)

(register-commands :fare-scripts/network)
