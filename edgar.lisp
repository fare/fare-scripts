#|
Script to download contents from EDGAR
(e.g. as base data on which to train various machine learning tools)

(asdf:make :load-quicklisp) (asdf:make :fare-scripts/edgar) (in-package :fare-scripts/edgar)

In ~/tmp/edgar/ I used:
  lftp ftp.sec.gov
  cd edgar
  mirror daily-index


FARE-SCRIPTS/EDGAR> (time (defparameter *d* (multiple-value-list (collect-index-data (get-all-compressed-form-indexes)))))
Evaluation took:
  239.139 seconds of real time
  221.767000 seconds of total run time (194.312000 user, 27.455000 system)
  [ Run times consist of 12.609 seconds GC time, and 209.158 seconds non-GC time. ]
  92.74% CPU
  32 lambdas converted
  430,443,370,423 processor cycles
  50,257,987,904 bytes consed

*D*
FARE-SCRIPTS/EDGAR> (car *d*)
16021766
FARE-SCRIPTS/EDGAR> (mapcar 'length (cdr *d*))
(696 612150 563489) ;; distinct type forms, distinct company names, distinct CIKs
|#

(uiop:define-package #:fare-scripts/edgar
  (:documentation "downloading EDGAR from ftp.sec.org")
  (:mix :common-lisp :uiop :fare-utils :inferior-shell
	:alexandria :split-sequence
	:org.mapcar.ftp.client
	;;:bordeaux-threads
	;;:drakma
	;;:cl-date-time-parser :local-time
	:optima :optima.ppcre))

(defpackage #:fare-scripts/edgar/form-types (:use))
(defpackage #:fare-scripts/edgar/company-names (:use))

(cl:in-package #:fare-scripts/edgar)

(declaim (optimize (speed 1) (safety 2) (debug 3)))

(defparameter *edgar-server* "ftp.sec.gov")
(defparameter *edgar-base* (subpathname (user-homedir-pathname) "tmp/"))

(defmacro with-edgar-ftp ((conn) &body body)
  `(with-ftp-connection (,conn :hostname *edgar-server*
			       :username "anonymous" :password "fare-scripts@"
			       :passive-ftp-p t) ,@body))

(defun get-file (path)
  (check-type path string)
  (let ((local (ensure-pathname
		path :namestring :unix
		:want-relative t :want-file t :ensure-absolute t :defaults *edgar-base*)))
    (ensure-directories-exist local)
    (when (file-exists-p local)
      (rename-file-overwriting-target local (add-pathname-suffix local "-OLD")))
    (with-edgar-ftp (c)
      (retrieve-file c path local))))

(defun compact-date->universal-time (date)
  (multiple-value-bind (year md) (floor date 10000)
    (multiple-value-bind (month date) (floor md 100)
      (assert (<= 1900 year 9999))
      (assert (<= 1 month 12))
      (assert (<= 1 date 31))
      (encode-universal-time 0 0 0 date month year 0))))

(defun universal-time->compact-date (universal-time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time 0)
    (assert (zerop second))
    (assert (zerop minute))
    (assert (zerop hour))
    (assert (<= 1900 year 9999))
    (+ (* 10000 year) (* 100 month) date)))

(defun normalize-form-type (form-type)
  (symbol-name (intern form-type (load-time-value (find-package* '#:fare-scripts/edgar/form-types)))))

(defun normalize-company-name (name)
  (symbol-name (intern name (load-time-value (find-package* '#:fare-scripts/edgar/company-names)))))

(defun canonicalize-index-fields (index-fields)
  (destructuring-bind (&key form-type company-name cik date path) index-fields
    (append form-type company-name cik date path)))

(defun parse-index-line (canonical-index-fields line)
  (flet ((get-field ()
	   (string-trim " " (subseq line (pop canonical-index-fields) (pop canonical-index-fields)))))
    (let* ((form-type (get-field))
	   (company-name (get-field))
	   (cik0 (get-field))
	   (cik (parse-integer cik0))
	   (date0 (get-field))
	   (date (unless (emptyp date0) (parse-integer date0)))
	   (path (get-field)))
      (assert (<= (length form-type) 11))
      (assert (<= (length company-name) 61))
      (assert (<= 0 cik 9999999999))
      (when date ;; NB: big Y10K bug if USG still exists by then.
	(assert (<= 19800101 date 99991231)))
      (ematch path
	((ppcre "^(?:edgar/)?data[0-9]*/([0-9]{1,10}|[.]deleted)/([0-9]{10})-([0-9]{2})-([0-9]{6})[.]txt$"
		cik1 id1 year id2)
	 (let ((yr (parse-integer year))
	       (i1 (parse-integer id1))
	       (i2 (parse-integer id2))
	       (pcik (unless (equal cik1 ".deleted") (parse-integer cik1))))
	   #|
	   ;; This happens more often than not before 2011:
	   (unless (equal pcik cik)
	     ;; Sometimes, we have (equal pcik i1). Sometimes, not even.
	     (warn "path for CIK doesn't match: ~A" line))
	   |#
	   #|
	   ;; Most of the time, year is either year of filing (same as date of index file)
	   ;; or year of document (for back-filing).
	   ;; But sometimes not.
	   (assert (or (= (mod (- file-year 80) 100) (mod (- yr 80) 100))
	               (= yr (mod (floor date 10000) 100))))
	   |#
	   (list form-type company-name cik date pcik i1 yr i2)))))))

(defun parse-index-stream (input)
  (nest
   (let ((line-buffer nil)))
   (labels ((peek-line () (or line-buffer (setf line-buffer (read-line input nil))))
	    (get-line () (if line-buffer
			     (prog1 line-buffer (setf line-buffer nil))
			     (read-line input nil)))))
   (let* ((headers
	   (loop :for line = (string-trim " " (peek-line))
	     :until (emptyp line)
	     :do (get-line) :collect
	     (match line
	       ((ppcre "^([^:]+): +(.*)$" key value) (cons key value))
	       (_ (error "Bad header: ~A" line)))))
	  ;;(last-data-received
	  ;; (parse-date-time (cdr (assoc "Last Data Received" headers :test 'equal))))
	  )
     ;; Skip empty lines
     (loop :for line = (string-trim " " (peek-line))
       :while (emptyp line) :do (get-line)))
   (let* ((index-fields
	   ;; Skip more headers
	   ;; TODO: these are only valid for the form*.idx indexes. Support the master*.idx too.
	   (canonicalize-index-fields
	    (let ((line (string-trim " " (get-line))))
	      (cond
		((equal line "Form Type   Company Name                                                  CIK")
		 (assert (equal (get-line) "      Date Filed  File Name"))
		 '(:form-type (0 12)
		   :company-name (12 74)
		   :cik (74 86)
		   :date (86 98)
		   :path (98 nil)))
		((equal line "Form Type   Company Name                                                  CIK         Date Filed  File Name")
		 '(:form-type (0 12)
		   :company-name (12 74)
		   :cik (74 86)
		   :date (86 98)
		   :path (98 nil)))
		(t
		 (error "Unrecognized index file type: ~A" line))))))
	  (line (get-line)))
     (assert (every (lambda (c) (eql c #\-)) line))
     (assert (<= 120 (length line))))

   #|(multiple-value-bind (ignore-seconds ignore-minutes ignore-hours
					file-date file-month file-year
					ignore-weekday ignore-dst ignore-tz)
       (decode-universal-time last-data-received 0)
     (declare (ignore ignore-seconds ignore-minutes ignore-hours
		      file-date file-month file-year
		      ignore-weekday ignore-dst ignore-tz)))|#
   (let ((entries
	  (loop :for line = (get-line) :while line :collect (parse-index-line index-fields line)))))
   (list headers entries)))

(defun parse-index-file (path &key (compressed (equal (pathname-type path) "gz")))
  (let ((path (ensure-pathname
	       path :namestring :unix :want-file t
	       :ensure-absolute t :defaults *edgar-base*)))
    (if compressed
	(run-program '("gzip" "-d")
		     :input path :output #'parse-index-stream :error-output nil
		     :external-format :latin1)
	(with-input-file (input path :external-format :latin1)
	  (parse-index-stream input)))))

(defun collect-index-data (index-files)
  (let ((count 0)
        (form-types (make-hash-table :test 'equal))
        (company-names (make-hash-table :test 'equal))
        (ciks (make-hash-table :test 'equal)))
    (loop :for file :in index-files
      :for (() entries) = (parse-index-file file) :do
      (loop :for (form-type company-name cik . ()) :in entries :do
	(incf count)
	(incf (gethash form-type form-types 0))
	(incf (gethash company-name company-names 0))
	(incf (gethash cik ciks 0))))
    (values
     count
     (sort (hash-table-alist form-types) #'< :key #'cdr)
     (sort (hash-table-alist company-names) #'< :key #'cdr)
     (sort (hash-table-alist ciks) #'< :key #'cdr))))

(defvar *all-entries* nil)
(defvar *by-form-type* nil)
(defvar *by-company-name* nil)
(defvar *by-cik* nil)

(defun read-indexes (index-files &key (filter t))
  (setf *all-entries* '()
	*by-form-type* (make-hash-table :test 'equal)
	*by-company-name* (make-hash-table :test 'equal)
	*by-cik* (make-hash-table :test 'equal))
  (loop :for file :in index-files :for i :from 1
    :for (() entries) = (parse-index-file file) :do
    (format t "~&Reading index file #~D ~A~%" i file)
    (when (= 1 (mod i 100))
      (sb-ext:gc :full t)
      (room))
    (loop :for entry :in entries
      :for (form-type company-name cik . ()) = entry
      :when (call-function filter entry) :do
      (push entry *all-entries*)
      (push entry (gethash form-type *by-form-type* nil))
      (push entry (gethash company-name *by-company-name* nil))
      (push entry (gethash cik *by-cik* nil)))))

(defun get-all-compressed-form-indexes ()
  (directory (subpathname *edgar-base* #p"edgar/daily-index/????/QTR?/form*.idx.gz")))

(defun initialize-indexes (&key (filter t))
  (multiple-value-setq (*all-entries* *by-form-type* *by-company-name* *by-cik*)
    (read-indexes (get-all-compressed-form-indexes) :filter filter))
  (values))

(defun summarize-index (table &key (comparator #'string<) (summarizer #'length))
  (sort (loop :for key :being :the :hash-keys :of table :using (:hash-value value)
	  :collect (funcall summarizer value)) comparator))

(defun summarize-indexes ()
  (values
   (length *all-entries*)
   (summarize-index *by-form-type*)
   (summarize-index *by-company-name*)
   (summarize-index *by-cik* :comparator #'<)))

(defun entry-directory (entry)
  (nest
   (destructuring-bind (form-type company-name cik date pcik id1 year id2) entry
     (declare (ignore form-type company-name cik date)))
   (format nil "edgar/data/~D/~2,'0D/~10,'0D~2,'0D~6,'0D/"
	   pcik year id1 year id2)))

(defun entry-textfile (entry)
  (nest
   (destructuring-bind (form-type company-name cik date pcik id1 year id2) entry
     (declare (ignore form-type company-name cik date pcik)))
   (format nil "~10,'0D-~2,'0D-~6,'0D.txt" id1 year id2)))

(defun lftp-files (directory)
  (check-type directory string)
  (let ((lines (run/lines
		`(lftp ("ftp://" ,*edgar-server*) "-e"
		       ("cd /" ,directory " && ls ; quit"))
		:on-error nil)))
    ;;(ematch (first lines)
    ;;((ppcre "^cd ok, cwd=/(.*)$" cwd) (assert (equal cwd origin)) (pop lines)))
    (loop :for l :in lines
      :for skip = (nth-value 1 (split-sequence:split-sequence #\space l :count 8 :remove-empty-subseqs t))
      :collect (subseq l skip))))

(defun lftp-get (files)
  (run/i `(lftp ("ftp://" ,*edgar-server* "/") "-e"
		("get -c "
		 ,@(mapcar (lambda (file) (list " " file " -o " file ".tmp")) files)
		 " ; quit"))))

(defun in-edgar-base (file &key type)
  (subpathname *edgar-base* file :type type))

(defun retrieve-entry-files (entry &key (mode :lftp))
  (block nil
    (let* ((directory (entry-directory entry))
	   (local-dir (in-edgar-base directory :type :directory)))
      (when (directory-exists-p local-dir)
	(if-let ((local-files (directory-files local-dir)))
	  (when (loop :for file :in local-files :always
		  (and (equal (pathname-type file) "xz")
		       (not (string-suffix-p (pathname-name file) ".tmp"))))
	    ;; All files already downloaded!
	    (return (values (mapcar 'pathname-name local-files) nil)))))
      (labels
	  ((in-local-dir (x &key type)
	     (in-edgar-base (strcat directory x) :type type))
	   (body (list-files get-files)
	     (let* ((files (funcall list-files directory))
		    (interesting-files
		     (remove-if
		      (let ((textfile (entry-textfile entry)))
			(lambda (x) (or (equal x textfile) (string-suffix-p x ".zip"))))
		      files))
		    (new-files
		     (remove-if
		      (lambda (x) (file-exists-p (in-local-dir x :type "xz")))
		      interesting-files)))
	     (when new-files
	       (ignore-errors
		 (ensure-directories-exist (subpathname *edgar-base* directory))
		 (funcall get-files (mapcar (lambda (x) (strcat directory x)) new-files))
		 (run/i `("sh" "-c"
			       ("(xz "
				,@(mapcan
				   (lambda (x) (list (in-local-dir x :type "tmp") " "))
				   new-files)
				,@(mapcan
				   (lambda (x) (list
						" ; mv -f "
						(in-local-dir (strcat x ".tmp.xz"))
						" "
						(in-local-dir x :type "xz")))
				   new-files)
				")&")))
		 (values interesting-files new-files))))))
	(ecase mode
	  (:cl-ftp
	   (with-edgar-ftp (c)
	     (body (lambda (dir)
		     (mapcar 'file-namestring (retrieve-filename-list c dir)))
		   (lambda (files)
		     (dolist (file files)
		       (retrieve-file c file (in-edgar-base file :type "tmp")))))))
	  (:lftp
	   (with-current-directory (*edgar-base*)
	     (body #'lftp-files #'lftp-get))))))))

(defparameter *S&P500-2016*
  (alist-hash-table
   '(("MMM" . 66740) ("ABT" . 1800) ("ABBV" . 1551152) ("ACN" . 1467373)
     ("ATVI" . 718877) ("ADBE" . 796343) ("ADT" . 1546640) ("AAP" . 1158449)
     ("AES" . 874761) ("AET" . 1122304) ("AFL" . 4977) ("AMG" . 1004434)
     ("A" . 1090872) ("GAS" . 1004155) ("APD" . 2969) ("ARG" . 804212)
     ("AKAM" . 1086222) ("AA" . 4281) ("AGN" . 1578845) ("ALXN" . 899866)
     ("ALLE" . 1579241) ("ADS" . 1101215) ("ALL" . 899051) ("GOOGL" . 1288776)
     ("GOOG" . 1288776) ("MO" . 764180) ("AMZN" . 1018724) ("AEE" . 1002910)
     ("AAL" . 6201) ("AEP" . 4904) ("AXP" . 4962) ("AIG" . 5272) ("AMT" . 1053507)
     ("AWK" . 1410636) ("AMP" . 820027) ("ABC" . 1140859) ("AME" . 1037868)
     ("AMGN" . 318154) ("APH" . 820313) ("APC" . 773910) ("ADI" . 6281)
     ("AON" . 315293) ("APA" . 6769) ("AIV" . 922864) ("AAPL" . 320193)
     ("AMAT" . 6951) ("ADM" . 7084) ("AIZ" . 1267238) ("T" . 732717)
     ("ADSK" . 769397) ("ADP" . 8670) ("AN" . 350698) ("AZO" . 866787)
     ("AVGO" . 1649338) ("AVB" . 915912) ("AVY" . 8818) ("BHI" . 808362)
     ("BLL" . 9389) ("BAC" . 70858) ("BK" . 1390777) ("BCR" . 9892)
     ("BXLT" . 1620546) ("BAX" . 10456) ("BBT" . 92230) ("BDX" . 10795)
     ("BBBY" . 886158) ("BRK-B" . 1067983) ("BBY" . 764478) ("BIIB" . 875045)
     ("BLK" . 1364742) ("HRB" . 12659) ("BA" . 12927) ("BWA" . 908255)
     ("BXP" . 1037540) ("BSX" . 885725) ("BMY" . 14272) ("BF-B" . 14693)
     ("CHRW" . 1043277) ("CA" . 356028) ("CVC" . 1053112) ("COG" . 858470)
     ("CPB" . 16732) ("COF" . 927628) ("CAH" . 721371) ("HSIC" . 1000228)
     ("KMX" . 1170010) ("CCL" . 815097) ("CAT" . 18230) ("CBG" . 1138118)
     ("CBS" . 813828) ("CELG" . 816284) ("CNC" . 1071739) ("CNP" . 1130310)
     ("CTL" . 18926) ("CERN" . 804753) ("CF" . 1324404) ("SCHW" . 316709)
     ("CHK" . 895126) ("CVX" . 93410) ("CMG" . 1058090) ("CB" . 896159)
     ("CHD" . 313927) ("CI" . 701221) ("XEC" . 1168054) ("CINF" . 20286)
     ("CTAS" . 723254) ("CSCO" . 858877) ("C" . 831001) ("CTXS" . 877890)
     ("CLX" . 21076) ("CME" . 1156375) ("CMS" . 811156) ("COH" . 1116132)
     ("KO" . 21344) ("CCE" . 1491675) ("CTSH" . 1058290) ("CL" . 21665)
     ("CPGX" . 1629995) ("CMCSA" . 1166691) ("CMA" . 28412) ("CAG" . 23217)
     ("CXO" . 1358071) ("COP" . 1163165) ("ED" . 1047862) ("STZ" . 16918)
     ("GLW" . 24741) ("COST" . 909832) ("CCI" . 1051470) ("CSRA" . 1646383)
     ("CSX" . 277948) ("CMI" . 26172) ("CVS" . 64803) ("DHI" . 882184)
     ("DHR" . 313616) ("DRI" . 940944) ("DVA" . 927066) ("DE" . 315189)
     ("DLPH" . 1521332) ("DAL" . 27904) ("XRAY" . 818479) ("DVN" . 1090012)
     ("DO" . 949039) ("DFS" . 1393612) ("DISCA" . 1437107) ("DISCK" . 1437107)
     ("DG" . 29534) ("DLTR" . 935703) ("D" . 715957) ("DOV" . 29905)
     ("DOW" . 29915) ("DPS" . 1418135) ("DTE" . 936340) ("DD" . 30554)
     ("DUK" . 1326160) ("DNB" . 1115222) ("ETFC" . 1015780) ("EMN" . 915389)
     ("ETN" . 1551182) ("EBAY" . 1065088) ("ECL" . 31462) ("EIX" . 827052)
     ("EW" . 1099800) ("EA" . 712515) ("EMC" . 790070) ("EMR" . 32604)
     ("ENDP" . 1593034) ("ETR" . 65984) ("EOG" . 821189) ("EQT" . 33213)
     ("EFX" . 33185) ("EQIX" . 1101239) ("EQR" . 906107) ("ESS" . 920522)
     ("EL" . 1001250) ("ES" . 72741) ("EXC" . 1109357) ("EXPE" . 1324424)
     ("EXPD" . 746515) ("ESRX" . 1532063) ("EXR" . 1289490) ("XOM" . 34088)
     ("FFIV" . 1048695) ("FB" . 1326801) ("FAST" . 815556) ("FRT" . 34903)
     ("FDX" . 1048911) ("FIS" . 1136893) ("FITB" . 35527) ("FSLR" . 1274494)
     ("FE" . 1031296) ("FISV" . 798354) ("FLIR" . 354908) ("FLS" . 30625)
     ("FLR" . 1124198) ("FMC" . 37785) ("FTI" . 1135152) ("FL" . 850209)
     ("F" . 37996) ("BEN" . 38777) ("FCX" . 831259) ("FTR" . 20520)
     ("GME" . 1326380) ("GPS" . 39911) ("GRMN" . 1121788) ("GD" . 40533)
     ("GE" . 40545) ("GGP" . 1496048) ("GIS" . 40704) ("GM" . 1467858)
     ("GPC" . 40987) ("GILD" . 882095) ("GS" . 886982) ("GT" . 42582)
     ("GWW" . 277135) ("HAL" . 45012) ("HBI" . 1359841) ("HOG" . 793952)
     ("HAR" . 800459) ("HRS" . 202058) ("HIG" . 874766) ("HAS" . 46080)
     ("HCA" . 860730) ("HCP" . 765880) ("HP" . 46765) ("HES" . 4447)
     ("HPE" . 1645590) ("HOLX" . 859737) ("HD" . 354950) ("HON" . 773840)
     ("HRL" . 48465) ("HST" . 1070750) ("HPQ" . 47217) ("HUM" . 49071)
     ("HBAN" . 49196) ("ITW" . 49826) ("ILMN" . 1110803) ("IR" . 1466258)
     ("INTC" . 50863) ("ICE" . 1571949) ("IBM" . 51143) ("IP" . 51434)
     ("IPG" . 51644) ("IFF" . 51253) ("INTU" . 896878) ("ISRG" . 1035267)
     ("IVZ" . 914208) ("IRM" . 1020569) ("JEC" . 52988) ("JBHT" . 728535)
     ("JNJ" . 200406) ("JCI" . 53669) ("JPM" . 19617) ("JNPR" . 1043604)
     ("KSU" . 54480) ("K" . 55067) ("KEY" . 91576) ("KMB" . 55785) ("KIM" . 879101)
     ("KMI" . 1506307) ("KLAC" . 319201) ("KSS" . 885639) ("KHC" . 1637459)
     ("KR" . 56873) ("LB" . 701985) ("LLL" . 1056239) ("LH" . 920148)
     ("LRCX" . 707549) ("LM" . 704051) ("LEG" . 58492) ("LEN" . 920760)
     ("LVLT" . 794323) ("LUK" . 96223) ("LLY" . 59478) ("LNC" . 59558)
     ("LLTC" . 791907) ("LMT" . 936468) ("L" . 60086) ("LOW" . 60667)
     ("LYB" . 1489393) ("MTB" . 36270) ("MAC" . 912242) ("M" . 794367)
     ("MNK" . 1567892) ("MRO" . 101778) ("MPC" . 1510295) ("MAR" . 1048286)
     ("MMC" . 62709) ("MLM" . 916076) ("MAS" . 62996) ("MA" . 1141391)
     ("MAT" . 63276) ("MKC" . 63754) ("MCD" . 63908) ("MHFI" . 64040)
     ("MCK" . 927653) ("MJN" . 1452575) ("WRK" . 1636023) ("MDT" . 1613103)
     ("MRK" . 310158) ("MET" . 1099219) ("KORS" . 1530721) ("MCHP" . 827054)
     ("MU" . 723125) ("MSFT" . 789019) ("MHK" . 851968) ("TAP" . 24545)
     ("MDLZ" . 1103982) ("MON" . 1110783) ("MNST" . 865752) ("MCO" . 1059556)
     ("MS" . 895421) ("MOS" . 1285785) ("MSI" . 68505) ("MUR" . 717423)
     ("MYL" . 1623613) ("NDAQ" . 1120193) ("NOV" . 1021860) ("NAVI" . 1593538)
     ("NTAP" . 1002047) ("NFLX" . 1065280) ("NWL" . 814453) ("NFX" . 912750)
     ("NEM" . 1164727) ("NWSA" . 1564708) ("NWS" . 1564708) ("NEE" . 753308)
     ("NLSN" . 1492633) ("NKE" . 320187) ("NI" . 1111711) ("NBL" . 72207)
     ("JWN" . 72333) ("NSC" . 702165) ("NTRS" . 73124) ("NOC" . 1133421)
     ("NRG" . 1013871) ("NUE" . 73309) ("NVDA" . 1045810) ("ORLY" . 898173)
     ("OXY" . 797468) ("OMC" . 29989) ("OKE" . 1039684) ("ORCL" . 1341439)
     ("OI" . 812074) ("PCAR" . 75362) ("PH" . 76334) ("PDCO" . 891024)
     ("PAYX" . 723531) ("PYPL" . 1633917) ("PNR" . 77360) ("PBCT" . 1378946)
     ("PEP" . 77476) ("PKI" . 31791) ("PRGO" . 1585364) ("PFE" . 78003)
     ("PCG" . 1004980) ("PM" . 1413329) ("PSX" . 1534701) ("PNW" . 764622)
     ("PXD" . 1038357) ("PBI" . 78814) ("PNC" . 713676) ("RL" . 1037038)
     ("PPG" . 79879) ("PPL" . 922224) ("PX" . 884905) ("CFG" . 759944)
     ("PCLN" . 1075531) ("PFG" . 1126328) ("PG" . 80424) ("PGR" . 80661)
     ("PLD" . 1045609) ("PRU" . 1137774) ("PEG" . 788784) ("PSA" . 1393311)
     ("PHM" . 822416) ("PVH" . 78239) ("QRVO" . 1604778) ("PWR" . 1050915)
     ("QCOM" . 804328) ("DGX" . 1022079) ("RRC" . 315852) ("RTN" . 1047122)
     ("O" . 726728) ("RHT" . 1087423) ("REGN" . 872589) ("RF" . 1281761)
     ("RSG" . 1060391) ("RAI" . 1275283) ("RHI" . 315213) ("ROK" . 1024478)
     ("COL" . 1137411) ("ROP" . 882835) ("ROST" . 745732) ("RCL" . 884887)
     ("R" . 85961) ("CRM" . 1108524) ("SNDK" . 1000180) ("SCG" . 754737)
     ("SLB" . 87347) ("SNI" . 1430602) ("STX" . 1137789) ("SEE" . 1012100)
     ("SRE" . 1032208) ("SHW" . 89800) ("SIG" . 832988) ("SPG" . 1063761)
     ("SWKS" . 4127) ("SLG" . 1040971) ("SJM" . 91419) ("SNA" . 91440)
     ("SO" . 92122) ("LUV" . 92380) ("SWN" . 7332) ("SE" . 1373835)
     ("STJ" . 203077) ("SWK" . 93556) ("SPLS" . 791519) ("SBUX" . 829224)
     ("HOT" . 316206) ("STT" . 93751) ("SRCL" . 861878) ("SYK" . 310764)
     ("STI" . 750556) ("SYMC" . 849399) ("SYF" . 1601712) ("SYY" . 96021)
     ("TROW" . 1113169) ("TGT" . 27419) ("TEL" . 1385157) ("TE" . 350563)
     ("TGNA" . 39899) ("TDC" . 816761) ("TSO" . 50104) ("TXN" . 97476)
     ("TXT" . 217346) ("HSY" . 47111) ("TRV" . 86312) ("TMO" . 97745)
     ("TIF" . 98246) ("TWX" . 1105705) ("TWC" . 1377013) ("TJX" . 109198)
     ("TMK" . 320335) ("TSS" . 721683) ("TSCO" . 916365) ("RIG" . 1451505)
     ("TRIP" . 1526520) ("FOXA" . 1308161) ("FOX" . 1308161) ("TSN" . 100493)
     ("TYC" . 833444) ("UDR" . 74208) ("ULTA" . 1403568) ("USB" . 36104)
     ("UA" . 1336917) ("UNP" . 100885) ("UAL" . 100517) ("UNH" . 731766)
     ("UPS" . 1090727) ("URI" . 1067701) ("UTX" . 101829) ("UHS" . 352915)
     ("UNM" . 5513) ("URBN" . 912615) ("VFC" . 103379) ("VLO" . 1035002)
     ("VAR" . 203527) ("VTR" . 740260) ("VRSN" . 1014473) ("VRSK" . 1442145)
     ("VZ" . 732712) ("VRTX" . 875320) ("VIAB" . 1339947) ("V" . 1403161)
     ("VNO" . 899689) ("VMC" . 1396009) ("WMT" . 104169) ("WBA" . 1618921)
     ("DIS" . 1001039) ("WM" . 823768) ("WAT" . 1000697) ("ANTM" . 1156039)
     ("WFC" . 72971) ("HCN" . 766704) ("WDC" . 106040) ("WU" . 1365135)
     ("WY" . 106535) ("WHR" . 106640) ("WFM" . 865436) ("WMB" . 107263)
     ("WLTW" . 1140536) ("WEC" . 783325) ("WYN" . 1361658) ("WYNN" . 1174922)
     ("XEL" . 72903) ("XRX" . 108772) ("XLNX" . 743988) ("XL" . 875159)
     ("XYL" . 1524472) ("YHOO" . 1011006) ("YUM" . 1041061) ("ZBH" . 1136869)
     ("ZION" . 109380) ("ZTS" . 1555280))
   :test 'equal))

(defun reverse-hash-table (table &key test)
  (alist-hash-table
   (mapcar (lambda (x) (cons (cdr x) (car x))) (hash-table-alist table))
   :test test))

(defparameter *cik-sp500*
  (reverse-hash-table *S&P500-2016* :test 'equal))

;; NB: The tickers were retrieved with a python program using finsymbols.
;; The CIKs were retrieved with this function, using drakma:
#|
(defun cik-from-ticker (ticker)
  (check-type ticker string)
  (let* ((ticker0 (remove #\- ticker))
	 (result (http-request "http://www.sec.gov/cgi-bin/browse-edgar" ;; uses drakma:
			       :parameters `(("CIK" . ,ticker0) ("action" . "getcompany")))))
    (match result
      ((ppcre "CIK=([0-9]{10})&" cik) (values (parse-integer cik))))))
|#

(defun entry-sp500-p (entry)
  (gethash (third entry) *cik-sp500*))

(defun read-sp500-index ()
  (read-indexes (get-all-compressed-form-indexes) :filter 'entry-sp500-p))

(defun coerce-to-cik (x)
  (etypecase x
    (keyword (values (gethash (symbol-name x) *S&P500-2016*)))
    (string (values (gethash x *S&P500-2016*)))
    (integer x)))

(defun retrieve-cik-files (x)
  (map () 'retrieve-entry-files (gethash (coerce-to-cik x) *by-cik*)))

(defun retrieve-sp500-files ()
  (loop :for cik :in (hash-table-values *S&P500-2016*) :do (retrieve-cik-files cik)))

(defun number-of-digits (n)
  ;; This function works well enough for all the numbers we care about,
  ;; but let's not let it silently give the wrong answer.
  (assert (< (integer-length (integer-length n)) 50)) ;; NB: a double's mantissa is 53 bits long.
  (ceiling (log (+ 1 n) 10d0)))

(defun n-out-of-m (n m &optional s)
  (with-output (s)
    (format s "[~v,' D/~D]" (number-of-digits m) n m)))

(defun retrieve-entries (l)
  (loop
    :with ll = (length l)
    :for i :from 0
    :for e :in l :do
    (format t "~A ~A~%" (n-out-of-m i ll) (entry-directory e))
    (retrieve-entry-files e)))

(defun retrieve-10k-files ()
  (retrieve-entries (gethash "10-K" *by-form-type*)))

(defun retrieve-google-files ()
  (retrieve-entries (gethash (coerce-to-cik :goog) *by-cik*)))
