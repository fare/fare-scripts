;;; -*- mode: Lisp; coding: utf-8 -*-
;;; vicode - support for computing in vietnamese
;;;
;;; This contains code as translated from from my original perl script vncode
;;; Copyright (c) 1996-1998, 2016 François-René "Faré" Rideau DDan(.ng-Vu~ Ba^n
;;;
;;; TODO: Use byte streams instead of character pseudo-streams assuming
;;; one-byte character and/or abusing the latin1 encoding.

(uiop:define-package :fare-scripts/vicode
  (:use :common-lisp :uiop :fare-utils :optima :optima.ppcre :cl-unicode)
  (:export
   #:*viet-dict* #:split-viet-dict-file #:dict-key #:run-sort-program
   #:sort-viet-dict
   #:init-vichar-tables
   #:viletter-from-vichar #:full-viletter-from-full-vichar
   #:vichar-from-viletter #:full-vichar-from-full-viletter
   #:new-viletter-tables #:full-viletter-get
   #:full-viletter-from-viqr #:full-vichar-from-viqr
   #:viqr-from-full-viletter
   #:ascii-vowel-p #:full-vichar-simple-p
   #:*viqr-quote*
   #:init-viqr-tables #:get-viqr-char #:put-viqr-char #:viqr-from-full-vichar
   #:init-viscii11-tables #:get-viscii11-char #:put-viscii11-char #:viscii11-from-full-vichar
   #:init-viscii10-tables #:get-viscii10-char #:put-viscii10-char
   #:init-vietword-tables #:get-vietword-char #:put-vietword-char
   #:init-unicode-tables #:get-unicode-char #:put-unicode-char
   #:init-ndn-tables #:get-ndn-char #:put-ndn-char
   #:init-sort-tables #:get-sort-char #:put-sort-char
   #:vnsort #:vi-sortable-string #:transcode
   #:unicode-from-viqr #:viqr-from-unicode
   ))
(in-package :fare-scripts/vicode)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; We want to keep this file sorted, in particular
(defparameter *viet-dict*
  (subpathname (user-homedir-pathname) "fare/scratch/viet"))

;;; The file has two sections; only the first one needs to be sorted; this splits them.
(defun split-viet-dict-file ()
  (let* ((all (with-input-file (i *viet-dict*) (slurp-stream-lines i)))
         (end-dict (position-if (lambda (line) (string-prefix-p "----" line)) all)))
    (values (subseq all 0 end-dict) (subseq all end-dict))))

;;; This extracts the key that needs to be sorted in Vietnamese dictionary order
(defun dict-key (line)
  (match line
    ((ppcre "[0-5] [0-5] ([(][^()]+[)] )*([^()=]+) ([(][^()]+[)] )*= .*" _ word _)
     word)
    (_
     (error "Invalid line: ~A~%" line))))

;;; First attempt: use the glibc unicode support for collation.
(defun run-sort-program (entries &key key sort)
  (let* ((vector (coerce entries 'vector))
         (len (length vector))
         (klen (ceiling (log len 10)))
         (keyfun (or key #'identity)))
    (labels ((prepare-input (s)
               (loop :for line :across vector
                 :for i :from 0
                 :do (format s "~v,'0D ~A~%" klen i (funcall keyfun line))))
             (process-output (s)
               (loop :for line = (read-line s nil nil)
                 :for i = (when line (parse-integer line :junk-allowed t))
                 :while line
                 :collect (aref vector i))))
      (run-program (or sort '("sort" "-sk2"))
                   :input #'prepare-input
                   :output #'process-output
                   :error-output t))))

;; Problem: it doesn't sort word by word by collates all words into one before it sorts!!!!!
(defun sort-with-libc (lines &key (key #'identity))
  (run-sort-program lines :key key :sort `("/usr/bin/env" "LC_ALL=vi_VN.UTF-8" "sort" "-sk2")))

(defun sort-with-vicode (lines &key (key #'identity))
  (sort (copy-list lines) 'string< :key (lambda (line) (vi-sortable-string (funcall key line)))))

(defun sort-viet-dict ()
  (multiple-value-bind (words appendix) (split-viet-dict-file)
    (format t "~{~A~%~}"
            (append (sort-with-vicode words :key 'dict-key) appendix))))


;;;; ENCODING DATABASE

;; The VIQR/VISCII 1.1 table was extracted from Trichlor's report with jed
;; The VIETWORD table was extracted and intermixed
;; from an earlier 8088 asm program with perl
;; For NDN's Tintuc encoding, see below

(defparameter *vi-table*
  #(;; VIQR 1.1, Unicode, VISCII 1.1, VIETWORD 1.10, TCVN
    #(#("A"   #x41 #x41 #x41)
      #("A`"  #xc0 #x60  -1)
      #("A?"  #xc4 #x7c  -1)
      #("A~"  #xc3 #x7d  -1)
      #("A'"  #xc1 #x5e  -1)
      #("A."  #x80 #x7e  -1))
    #(#("A("  #xc5 #xfc #xa1)
      #("A(`" #x82 #xb1  -1)
      #("A(?" #x02 #xb2  -1)
      #("A(~" #x05 #xb3  -1)
      #("A('" #x81 #xb0  -1)
      #("A(." #x83 #x7b  -1))
    #(#("A^"  #xc2 #xfd #xa2)
      #("A^`" #x85 #x87  -1)
      #("A^?" #x86 #x8c  -1)
      #("A^~" #x06 #x90  -1)
      #("A^'" #x84 #x80  -1)
      #("A^." #x87 #xaf  -1))
    #(#("E"   #x45 #x45 #x45)
      #("E`"  #xc8 #xb5  -1)
      #("E?"  #xcb #xb6  -1)
      #("E~"  #x88 #xb7  -1)
      #("E'"  #xc9 #xb4  -1)
      #("E."  #x89 #xb8  -1))
    #(#("E^"  #xca #xfe #xa3)
      #("E^`" #x8b #xba  -1)
      #("E^?" #x8c #xbb  -1)
      #("E^~" #x8d #xbc  -1)
      #("E^'" #x8a #xb9  -1)
      #("E^." #x8e #xbd  -1))
    #(#("I"   #x49 #x49 #x49)
      #("I`"  #xcc #xbf  -1)
      #("I?"  #x9b #xc0  -1)
      #("I~"  #xce #xc1  -1)
      #("I'"  #xcd #xbe  -1)
      #("I."  #x98 #xc2  -1))
    #(#("O"   #x4f #x4f #x4f)
      #("O`"  #xd2 #xc4  -1)
      #("O?"  #x99 #xc5  -1)
      #("O~"  #xa0 #xc6  -1)
      #("O'"  #xd3 #xc3  -1)
      #("O."  #x9a #xc7  -1))
    #(#("O^"  #xd4 #xf9 #xa4)
      #("O^`" #x90 #xc9  -1)
      #("O^?" #x91 #xca  -1)
      #("O^~" #x92 #xcb  -1)
      #("O^'" #x8f #xc8  -1)
      #("O^." #x93 #xcc  -1))
    #(#("O+"  #xb4 #xfa #xa5)
      #("O+`" #x96 #xce #xea)
      #("O+?" #x97 #xcf  -1)
      #("O+~" #xb3 #xd0  -1)
      #("O+'" #x95 #xcd  -1)
      #("O+." #x94 #xd1  -1))
    #(#("U"   #x55 #x55 #x55)
      #("U`"  #xd9 #xd3  -1)
      #("U?"  #x9c #xd4  -1)
      #("U~"  #x9d #xd5  -1)
      #("U'"  #xda #xd2  -1)
      #("U."  #x9e #xd6  -1))
    #(#("U+"  #xbf #xfb #xa6)
      #("U+`" #xbb #xd8  -1)
      #("U+?" #xbc #xd9  -1)
      #("U+~" #xff #xda  -1)
      #("U+'" #xba #xd7  -1)
      #("U+." #xb9 #xdb  -1))
    #(#("Y"   #x59 #x59 #x59)
      #("Y`"  #x9f #xdd  -1)
      #("Y?"  #x14 #xde #x14)
      #("Y~"  #x19 #xdf #x19)
      #("Y'"  #xdd #xdc  -1)
      #("Y."  #x1e #x40  -1))
    #(#("a"   #x61 #x61 #x61)
      #("a`"  #xe0 #x85 #xb5)
      #("a?"  #xe4 #xe0 #xb6)
      #("a~"  #xe3 #xe1 #xb7)
      #("a'"  #xe1 #xa0 #xb8)
      #("a."  #xd5 #xe2 #xb9))
    #(#("a("  #xe5 #xe8 #xa8)
      #("a(`" #xa2 #xea #xbb)
      #("a(?" #xc6 #xeb #xbc)
      #("a(~" #xc7 #xec #xbd)
      #("a('" #xa1 #xe9 #xbe)
      #("a(." #xa3 #xed #xc6))
    #(#("a^"  #xe2 #x83 #xa9)
      #("a^`" #xa5 #xe4 #xc7)
      #("a^?" #xa6 #xe5 #xc8)
      #("a^~" #xe7 #xe6 #xc9)
      #("a^'" #xa4 #xe3 #xca)
      #("a^." #xa7 #xe7 #xcb))
    #(#("e"   #x65 #x65 #x65)
      #("e`"  #xe8 #x8a #xcc)
      #("e?"  #xeb #x81  -1)
      #("e~"  #xa8 #x84 #xcf)
      #("e'"  #xe9 #x82 #xd0)
      #("e."  #xa9 #x86 #xd1))
    #(#("e^"  #xea #x88 #xaa)
      #("e^`" #xab #x8b #xd2)
      #("e^?" #xac #x8e #xd3)
      #("e^~" #xad #x8f #xd4)
      #("e^'" #xaa #x89 #xd5)
      #("e^." #xae #xac #xd6))
    #(#("i"   #x69 #x69 #x69)
      #("i`"  #xec #x8d #xd7)
      #("i?"  #xef #xa9 #xd8)
      #("i~"  #xee #xaa #xdc)
      #("i'"  #xed #xa1 #xdd)
      #("i."  #xb8 #xab  -1))
    #(#("o"   #x6f #x6f #x6f)
      #("o`"  #xf2 #x95 #xdf)
      #("o?"  #xf6 #x91 #xe1)
      #("o~"  #xf5 #x92 #xe2)
      #("o'"  #xf3 #xa2 #xe3)
      #("o."  #xf7 #x94 #xe4))
    #(#("o^"  #xf4 #x93 #xab)
      #("o^`" #xb0 #x98 #xe5)
      #("o^?" #xb1 #x99 #xe6)
      #("o^~" #xb2 #x9a #xe7)
      #("o^'" #xaf #x96 #xe8)
      #("o^." #xb5 #x9b #xe9))
    #(#("o+"  #xbd #xf3 #xac)
      #("o+`" #xb6 #xf5  -1)
      #("o+?" #xb7 #xf6 #xeb)
      #("o+~" #xde #xf7 #xec)
      #("o+'" #xbe #xf4 #xed)
      #("o+." #xfe #xf8 #xee))
    #(#("u"   #x75 #x75 #x75)
      #("u`"  #xf9 #x97 #xef)
      #("u?"  #xfc #x9c #xf1)
      #("u~"  #xfb #x9d #xf2)
      #("u'"  #xfa #xa3 #xf3)
      #("u."  #xf8 #x9e #xf4))
    #(#("u+"  #xdf #x9f #xad)
      #("u+`" #xd7 #xa5 #xf5)
      #("u+?" #xd8 #xa6 #xf6)
      #("u+~" #xe6 #xa7 #xf7)
      #("u+'" #xd1 #xa4 #xf8)
      #("u+." #xf1 #xa8 #xf9))
    #(#("y"   #x79 #x79 #x79)
      #("y`"  #xcf #xef #xfa)
      #("y?"  #xd6 #xf0 #xfb)
      #("y~"  #xdb #xf1 #xfc)
      #("y'"  #xfd #xee #xfd)
      #("y."  #xdc #xf2 #xfe))
    #(#("D"   #x44 #x44 #x44))
    #(#("d"   #x64 #x64 #x64))
    #(#("DD"  #xd0 #xae #xa7))
    #(#("dd"  #xf0 #xad #xae))))

;; VISCII 1.1 to Unicode translation...
;; From cuong@haydn.Stanford.EDU (Cuong T. Nguyen)
(defparameter *viscii11-to-unicode*
  #(#x0000 #x0001 #x1eb2 #x0003 #x0004 #x1eb4 #x1eaa #x0007
    #x0008 #x0009 #x000a #x000b #x000c #x000d #x000e #x000f
    #x0010 #x0011 #x0012 #x0013 #x1ef6 #x0015 #x0016 #x0017
    #x0018 #x1ef8 #x001a #x001b #x001c #x001d #x1ef4 #x001f
    #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
    #x0028 #x0029 #x002a #x002b #x002c #x002d #x002e #x002f
    #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
    #x0038 #x0039 #x003a #x003b #x003c #x003d #x003e #x003f
    #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
    #x0048 #x0049 #x004a #x004b #x004c #x004d #x004e #x004f
    #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
    #x0058 #x0059 #x005a #x005b #x005c #x005d #x005e #x005f
    #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
    #x0068 #x0069 #x006a #x006b #x006c #x006d #x006e #x006f
    #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
    #x0078 #x0079 #x007a #x007b #x007c #x007d #x007e #x007f
    #x1ea0 #x1eae #x1eb0 #x1eb6 #x1ea4 #x1ea6 #x1ea8 #x1eac
    #x1ebc #x1eb8 #x1ebe #x1ec0 #x1ec2 #x1ec4 #x1ec6 #x1ed0
    #x1ed2 #x1ed4 #x1ed6 #x1ed8 #x1ee2 #x1eda #x1edc #x1ede
    #x1eca #x1ece #x1ecc #x1ec8 #x1ee6 #x0168 #x1ee4 #x1ef2
    #x00d5 #x1eaf #x1eb1 #x1eb7 #x1ea5 #x1ea7 #x1ea9 #x1ead
    #x1ebd #x1eb9 #x1ebf #x1ec1 #x1ec3 #x1ec5 #x1ec7 #x1ed1
    #x1ed3 #x1ed5 #x1ed7 #x1ee0 #x01a0 #x1ed9 #x1edd #x1edf
    #x1ecb #x1ef0 #x1ee8 #x1eea #x1eec #x01a1 #x1edb #x01af
    #x00c0 #x00c1 #x00c2 #x00c3 #x1ea2 #x0102 #x1eb3 #x1eb5
    #x00c8 #x00c9 #x00ca #x1eba #x00cc #x00cd #x0128 #x1ef3
    #x0110 #x1ee9 #x00d2 #x00d3 #x00d4 #x1ea1 #x1ef7 #x1eeb
    #x1eed #x00d9 #x00da #x1ef9 #x1ef5 #x00dd #x1ee1 #x01b0
    #x00e0 #x00e1 #x00e2 #x00e3 #x1ea3 #x0103 #x1eef #x1eab
    #x00e8 #x00e9 #x00ea #x1ebb #x00ec #x00ed #x0129 #x1ec9
    #x0111 #x1ef1 #x00f2 #x00f3 #x00f4 #x00f5 #x1ecf #x1ecd
    #x1ee5 #x00f9 #x00fa #x0169 #x1ee7 #x00fd #x1ee3 #x1eee))

;;; Hand-written database

(defparameter *viqr-vowels-uc* '("A" "A(" "A^" "E" "E^" "I" "O" "O^" "O+" "U" "U+" "Y"))
(defparameter *viqr-vowels-lc* (mapcar 'string-downcase *viqr-vowels-uc*))
(defparameter *n-vi-vowels* (length *viqr-vowels-uc*))
(defparameter *viqr-vowels* `#(,@*viqr-vowels-uc* ,@*viqr-vowels-lc*))
(defparameter *viqr-letters* `#(,@*viqr-vowels-uc* ,@*viqr-vowels-lc* "D" "d" "DD" "dd"))
(defparameter *n-vi-letters* (length *viqr-letters*))
(defparameter *viqr-accents* #("" "`" "?" "~" "'" ".")) ;; nationalist order: '`?~.
(defparameter *n-vi-accents* (length *viqr-accents*))

;;; In case we want to support telex mode someday...
(defparameter *telex-vowels-uc* #("A" "AW" "AA" "E" "EE" "I" "O" "OO" "OW" "U" "UW" "Y"))
(defparameter *telex-accents* #("" "S" "F" "R" "X" "J"))

(defparameter *vnencodings*
  '((("VIQR 1.1" "VIQR11" "Q11" "VIQR 1.0" "VIQR10" "Q10" "VIQR" "Q")
     init-viqr-tables
     get-viqr-char
     put-viqr-char)
    (("VISCII 1.1" "VISCII11" "V11" "V1" "1" "VISCII" "V")
     init-viscii11-tables
     get-viscii11-char
     put-viscii11-char)
    (("VIETWORD" "VW" "W")
     init-vietword-tables
     get-vietword-char
     put-vietword-char)
    (("NDN" "N" "TINTUC")
     init-ndn-tables
     get-ndn-char
     put-ndn-char)
    (("SORT" "S" "VNSORT" "SORTVN")
     init-sort-tables
     get-sort-char
     put-sort-char)
    (("VISCII 1.0" "VISCII10" "V10" "V0" "0")
     init-viscii10-tables
     get-viscii10-char
     put-viscii10-char)
    (("UNICODE" "UTF-8" "UTF8" "ISO10646" "10646" "UNI" "U")
     init-unicode-tables
     get-utf8-char
     put-utf8-char)))

(defparameter *vn-encoding-table*
  (let ((table (make-hash-table :test 'equal)))
    (loop :for encoding :in *vnencodings* :do
      (loop :for name :in (first encoding) :do
        (setf (gethash name table) encoding)))))


;;; Internal Tables

;; A "viletter" is an index in the above list of letters.
;;
;; A "full-viletter" is a pair of a viletter and an accent number (or 0 if not a vi vowel).
;;
;; A "vichar" is a pair of two things: an ASCII character and
;; the index number of the vietnamese letter among those whose VIQR encoding begins
;; with the given ASCII character if there are many, or NIL if there is only one.
;;
;; A "full-vichar" is a pair of a pair of a vichar and the
;; index number of the vietnamese accent used if the letter is a vowel, or 0 if not.

;;; Internal Database for letter/number <-> char association

(defparameter *viletter-to-vichar* nil
  "A table, for each viletter, of the corresponding vichar")
(defparameter *vichar-to-viletter* nil
  "A table, for every ASCII char that starts a vichar, the viletters")
(defparameter *muletter* nil
  "Table that to every index of viletter associates the index of the viletter with ^ if defined.")
(defparameter *nomuletter* nil
  "Table that to every index of viletter with ^ associates the index of the viletter without ^.")

(defun make-extensible-vector (&rest keys)
  (apply 'make-array '(0) :adjustable t :fill-pointer t keys))

(defun init-vichar-tables ()
  (setf *viletter-to-vichar* (make-array (list *n-vi-letters*))
        *vichar-to-viletter* (make-array '(128) :initial-element nil)
        *muletter* (make-hash-table :test 'equal)
        *nomuletter* (make-hash-table :test 'equal))
  (loop :for vowel :across "aeoAEO"
    :for vl-naked = (position (string vowel) *viqr-letters* :test 'equal)
    :for vl-mu = (position (strcat vowel "^") *viqr-letters* :test 'equal) :do
    (setf (gethash vl-naked *muletter*) vl-mu)
    (setf (gethash vl-mu *nomuletter*) vl-naked))
  (loop
    :for viletter :from 0              ; index of the viletter
    :for viqr :across *viqr-letters*   ; VIQR for the viletter
    :for char = (aref viqr 0)          ; first char
    :for code = (char-code char)       ; ASCII code for the first char
    :for () = (unless (aref *vichar-to-viletter* code)
                (setf (aref *vichar-to-viletter* code) (make-extensible-vector)))
    :for num = (fill-pointer (aref *vichar-to-viletter* code)) :do
    (setf (aref *viletter-to-vichar* viletter) (cons char num))
    (vector-push-extend viletter (aref *vichar-to-viletter* code))))

(defun viletter-from-vichar (vichar)
  (destructuring-bind (char . num) vichar
    (when num
      (aref (aref *vichar-to-viletter* (char-code char)) num))))

(defun full-viletter-from-full-vichar (full-vichar)
  (destructuring-bind (vichar . accent) full-vichar
    (cons (viletter-from-vichar vichar) accent)))

(defun vichar-from-viletter (viletter)
  (aref *viletter-to-vichar* viletter))

(defun full-vichar-from-full-viletter (full-viletter)
  (destructuring-bind (viletter . accent) full-viletter
    (cons (vichar-from-viletter viletter) accent)))

(defun full-viletter-get (table-position full-viletter)
  (destructuring-bind (viletter . accent) full-viletter
    (aref (aref (aref *vi-table* viletter) accent) table-position)))


;;; Understanding VIQR characters in the hand-written database
(defun full-viletter-from-viqr (viqr)
  (let* ((accent-index (position-if (lambda (accent) (string-suffix-p viqr accent))
                                    *viqr-accents* :from-end t))
         (accent (aref *viqr-accents* accent-index))
         (letter (subseq viqr 0 (- (length viqr) (length accent))))
         (letter-index (position letter *viqr-letters* :test 'equal)))
    (cons letter-index accent-index)))

(defun full-vichar-from-viqr (viqr)
  (destructuring-bind (viletter . accent) (full-viletter-from-viqr viqr)
    (cons (if viletter (vichar-from-viletter viletter) (cons (first-char viqr) nil))
          accent)))

(defparameter *viqr-position* 0)

(defun viqr-from-full-viletter (full-viletter)
  (full-viletter-get *viqr-position* full-viletter))

;;; Recognizing vietnamese text
(defun ascii-vowel-p (char)
  (and (find (char-downcase char) "aeiouy") t))

(defun full-vichar-simple-p (full-vichar)
  (destructuring-bind ((char . num) . accent) full-vichar
    (declare (ignore char))
    (and (null num) (null accent))))


;;;; VIQR encoding

;; Currently not really VIQR, but rather just VIR, with extensions.
;; As the quote character is not quite supported...
;; Also, should be able to select \v-ish mode,
;; independently on I/O
;; We should be able to configure this in lots of ways:
;; alternate character sets,
;; what non-canonical combinations to accept,
;; disabling extensions, etc.

;; .....

(defparameter *viqr-quote* nil) ;; could be #\\ instead

(defun init-viqr-tables () (values))

(defun get-viqr-char (peek next)
  (nest
   (if-let ((char (funcall peek))))
   (if (find char "Dd")
       (progn
         (funcall next)
         (let ((d (funcall peek)))
           (if (find d "Dd+-")  ; This is an extension to VIQR!
               (progn
                 (funcall next)
                 (cons (cons char 1) 0))
               (cons (cons char 0) 0)))))
   (if (not (ascii-vowel-p char))
       (progn
         (funcall next)
         (cons (cons char nil) nil)))
   (let* ((char-viletters (aref *vichar-to-viletter* (char-code char)))
          (ext (progn (funcall next) (funcall peek)))
          (num (or (position ext char-viletters
                             :key (lambda (viletter) (char (aref *viqr-letters* viletter) 1))
                             :start 1 :test 'equal) 0))
          (accent (progn (when (plusp num) (funcall next)) (funcall peek)))
          ;; what about alternate accents, such as / and \ for sach and huyen?
          (accent-num (if-let (num (position accent *viqr-accents* :test 'equal :key 'first-char))
                        (progn (funcall next) num)
                        0)))
     (cons (cons char num) accent-num))))

(defun viqr-from-full-vichar (full-vichar)
  (destructuring-bind (vichar . accent) full-vichar
    (destructuring-bind (char . num) vichar
      (if num
          (let* ((viletter (viletter-from-vichar vichar))
                 (string (strcat (aref *viqr-letters* viletter)
                                 (aref *viqr-accents* accent))))
            (if (< 1 (length string))
                (strcat *viqr-quote* string)
                string))
          (string char)))))

(defun put-viqr-char (put-char full-vichar)
  (when full-vichar
    (map () put-char (viqr-from-full-vichar full-vichar))))


;;;; One-char encodings

;;; Generic routines for encodings where every letter+accent symbol
;;; represented by a single byte char.

(defun make-inverse-vi-table (table-position make-entry)
  (loop :for viletter :from 0
    :for variants :across *vi-table* :do
    (loop :for accent :from 0
      :for variant :across variants
      :for code = (aref variant table-position)
      :when (<= 0 code) :do
      (funcall make-entry (cons viletter accent) code))))

(defun make-one-byte-table (table-position)
  (let ((to-vichar (make-array '(256) :initial-element nil)))
    (labels ((make-entry (full-viletter code)
               (setf (aref to-vichar code) (full-vichar-from-full-viletter full-viletter))))
      (make-inverse-vi-table table-position #'make-entry)
      to-vichar)))

(defun get-one-byte-char (to-vichar peek next)
  (let ((char (prog1 (funcall peek) (funcall next))))
    (and char (aref to-vichar (char-code char)))))

(defun convert-one-byte-char (table-position full-vichar)
  (when full-vichar
    (if (cdr full-vichar)
        (full-viletter-get table-position (full-viletter-from-full-vichar full-vichar))
        (caar full-vichar))))

(defun put-one-byte-char (viletter-to put-char full-vichar)
  (when full-vichar
    (funcall put-char (code-char (convert-one-byte-char viletter-to full-vichar)))))


;;;; VISCII 1.1 ENCODING

(defparameter *viscii11-to-full-vichar* nil)
(defparameter *viscii11-position* 1)

(defun init-viscii11-tables ()
  (unless *viscii11-to-full-vichar*
    (setf *viscii11-to-full-vichar* (make-one-byte-table *viscii11-position*))))

(defun get-viscii11-char (peek next)
  (get-one-byte-char *viscii11-to-full-vichar* peek next))

(defun put-viscii11-char (put-char full-vichar)
  (put-one-byte-char *viscii11-position* put-char full-vichar))

(defun viscii11-from-full-vichar (full-vichar)
  (convert-one-byte-char *viscii11-position* full-vichar))


;;;; VISCII 1.0 ENCODING

(defparameter *viscii10-to-full-vichar* nil)

(defun frob-viscii-code (code)
  ;; Exchange a. and O~, respectively D5 and A0 in viscii 1.1, and A0 and D5 in viscii 1.0
  (case code
    (#xD5 #xA0)
    (#xA0 #xD5)
    (otherwise code)))

(defun init-viscii10-tables ()
  (init-viscii11-tables))

(defun get-viscii10-char (peek next)
  (let ((char (prog1 (funcall peek) (funcall next))))
    (and char (aref *viscii11-to-full-vichar* (frob-viscii-code (char-code char))))))

(defun put-viscii10-char (put-char full-vichar)
  (when full-vichar
    (let ((code (frob-viscii-code (convert-one-byte-char *viscii11-position* full-vichar))))
      (funcall put-char (code-char code)))))


;;;; VIETWORD ENCODING

(defparameter *vietword-to-full-vichar* nil)
(defparameter *vietword-position* nil)

(defun get-vietword-char (peek next)
  ;; Vietword 1.10 inserts sometimes chr(#xFF) before end-of-lines,
  ;; with no apparent reason. It also ends files with \0's and ^Z's.
  ;; We remove all that crap, as it has no meaning anyway...
  (loop :while (find (funcall peek) #.(map 'string 'code-char '(#xFF #x00 #x1A))) :do
    (funcall next))
  (get-one-byte-char *vietword-to-full-vichar* peek next))

(defun put-vietword-char (put-char full-vichar)
  (put-one-byte-char *vietword-position* put-char full-vichar))

(defun init-vietword-tables ()
  (unless *vietword-to-full-vichar*
    (setf *vietword-to-full-vichar* (make-one-byte-table *vietword-position*))))


;;; UTF-8 encoding

(defparameter *unicode-to-viscii11* (make-hash-table :test 'equal))

(defun init-unicode-tables ()
  (loop :for i :below 256
    :for c :across *viscii11-to-unicode* :do
    (setf (gethash c *unicode-to-viscii11*) i))
  (init-viscii11-tables))

(defun get-unicode-char (peek next)
  (flet ((get-char () (prog1 (funcall peek) (funcall next))))
    (if-let (c (get-char))
      (block nil
        (if-let (viscii11 (gethash (char-code c) *unicode-to-viscii11*))
          (if-let (full-vichar (aref *viscii11-to-full-vichar* viscii11))
            (return full-vichar)))
        (cons (cons c nil) nil)))))

(defun unicode-from-full-vichar (full-vichar)
  (when full-vichar
    (if (cdr full-vichar)
        (code-char
         (aref *viscii11-to-unicode*
               (full-viletter-get *viscii11-position*
                                  (full-viletter-from-full-vichar full-vichar))))
        (caar full-vichar))))

(defun put-unicode-char (put-char full-vichar)
  (when full-vichar
    (funcall put-char (unicode-from-full-vichar full-vichar))))


;;;; Encoding from NDN's amateur MacIntosh fonts

;; Codes were guessed from binary dumps of word files.
;; There are errors, and more particular cases may exist... :( :( :(

(defun to-char-code (x)
  (if (characterp x) (char-code x) x))

(defparameter *ndn-vowels-uc*
  (mapcar 'to-char-code '(#\A 174 227 #\E 211 #\I #\O 231 243 #\U 200 #\Y)))
(defparameter *ndn-vowels-lc*
  (mapcar 'to-char-code '(#\a #\& 142 #\e #\" #\i #\o 141 #\! #\u 143 #\y)))
(defparameter *ndn-letters*
  `(,@*ndn-vowels-uc* ,@*ndn-vowels-lc* ,@(mapcar 'to-char-code '(#\D #\d #\F #\f))))
;; NB: Last two accent actually not known, repeated from the lower-case ones.
;; NDN accents are prefix
;; supplementary accents for in the lower-case case : mu+sach, mu+huyen on lowercase!
(defparameter *ndn-accents-uc*
  `(nil ,@(mapcar 'to-char-code '(228 239 #\% #\] 208 0 0))))
(defparameter *ndn-accents-lc*
  `(nil ,@(mapcar 'to-char-code '(157 #\^ #\% 172 #\_ 161 #\)))))
(defparameter *ndn-accents*
  (append *ndn-accents-uc* *ndn-accents-lc*))
(defparameter *n-ndn-accents* (length *ndn-accents-lc*))

(defparameter *viqr-to-ndn*
  '(("a`" . 137) ("e`" . 144) ("o`" . 153) ("u`" . 158)
    ("a~" . 138) ("e~" . 145) ("o~" . 145) ("u~" . 154)
    ("y~" . 159) ("I'" . 216) ("I`" . 232) ("I?" . 223)
    ("I~" . 234) ("I." . 235) ("i'" . 187) ("i`" . 233)
    ("i?" . 148) ("i~" . 236) ("i." . 149)))

;; Non-viet characters:
(defparameter *char-to-ndn*
  `((,(code-char #o12) . #o15)
    (#\F . #.(char-code #\+))
    (#\f . #.(char-code #\=))
    (#\) . 136)
    (#\" . 210)
    (#\= . 173)))

(defparameter *ndn-to-full-vichar* nil)
(defparameter *viletter-to-ndn* nil)

(defun init-ndn-tables ()
  (setf *ndn-to-full-vichar* (make-hash-table :test 'equal))
  (setf *viletter-to-ndn* (make-array (list *n-vi-letters*)))
  (loop :for l :below *n-vi-vowels* :do
    (setf (aref *viletter-to-ndn* l) (make-array *n-vi-accents*))
    (loop :for a :below *n-vi-accents* :do
      (flet ((register (l a nl na)
               (setf (aref (aref *viletter-to-ndn* l) a)
                     (map 'string 'code-char
                          (list (aref *ndn-accents* na) (aref *ndn-letters* nl))))))
        (register l a l a)
        (let ((ll (+ l *n-vi-vowels*))
              (la (+ a *n-vi-accents*)))
          (if (and (< 0 a) (< a 3) (gethash ll *nomuletter*))
              (register ll a (gethash ll *nomuletter*) (+ a 5))
              (register ll a ll la))))))

  (loop :for (viqr . ndn) :in *viqr-to-ndn*
    :for full-vichar = (full-vichar-from-viqr viqr)
    :for (vichar . accent) = full-vichar :do
    (setf (aref *ndn-to-full-vichar* ndn) full-vichar)
    (setf (aref (aref *viletter-to-ndn* (viletter-from-vichar vichar)) accent) ndn))

  (loop :for (char . ndn) :in *char-to-ndn* :do
    (setf (aref *ndn-to-full-vichar* ndn) (cons (cons char nil) nil))))

(defun get-ndn-char (peek next)
  (flet ((get-char () (prog1 (funcall peek) (funcall next))))
    (let* ((acc 0)
           (mu nil)
           (char (get-char)))
      (loop :for azz = (and char (position (char-code char) *ndn-accents* :test 'equal))
        :while azz :do
        (setf acc (mod azz *n-ndn-accents*))
        (setf char (get-char))
        (when (<= *n-vi-accents* acc)
          (setf mu t)
          (decf acc 5)))
      (or (gethash (char-code char) *ndn-to-full-vichar*)
          (let ((viletter (position char *ndn-letters* :test 'equal)))
            (when viletter
              (when mu
                (setf viletter (gethash viletter *muletter*))))
            (cons (vichar-from-viletter viletter) acc))))))

(defun put-ndn-char (put-char full-vichar)
  (when full-vichar
    (destructuring-bind (vichar . accent) full-vichar
      (destructuring-bind (char . num) vichar
        (declare (ignore num))
        (if (full-vichar-simple-p full-vichar)
            (funcall put-char
                     (if-let (ndn (gethash char *char-to-ndn*))
                       (code-char ndn)
                       char))
            (map () put-char
                 (aref (aref *viletter-to-ndn* (viletter-from-vichar vichar)) accent)))))))


;;;; SORTABLE ENCODING

(defun init-sort-tables () (values))

;; Buffer for get-sort-char and pointer into said buffer
(defparameter *gsc-buffer* (make-extensible-vector))
(defparameter *gsc-pointer* 0)

(defun get-sort-char (peek next)
  (labels
      ((get-char () (prog1 (funcall peek) (funcall next)))
       (refill ()
         (adjust-array *gsc-buffer* '(0))
         (setf *gsc-pointer* 0)
         (let ((n-expected-accents 0))
           (loop :for char = (funcall peek)
             :while (and char (not (ascii-letter-p char))) :do
             (funcall next)
             (vector-push-extend (cons (cons char nil) nil) *gsc-buffer*))
           (loop :for char = (get-char)
             :for () = (unless char (return))
             :for code = (char-code char)
             :for viletter-p = (plusp (length (aref *vichar-to-viletter* code)))
             :for num = (when viletter-p
                          (funcall next)
                          (digit-char-p (funcall peek))) :do
             (when (ascii-vowel-p char) (incf n-expected-accents))
             (vector-push-extend (cons (cons char num) (when viletter-p 0)) *gsc-buffer*)
             (let ((char (funcall peek)))
               (unless (and char (ascii-letter-p char)) (return))))
           (loop :with ptr = -1
             :repeat n-expected-accents :do
             (loop :until (ascii-vowel-p (caar (aref *gsc-buffer* (incf ptr)))))
             (setf (cdr (aref *gsc-buffer* ptr)) (digit-char-p (get-char)))))))
    (block nil
      (when (= *gsc-pointer* (length *gsc-buffer*))
        (if (funcall peek)
            (refill)
            (return nil)))
      (aref *gsc-buffer* (post-incf *gsc-pointer*)))))

;; Buffers to put-sort-char
(defparameter *psc-letter-buffer* (make-extensible-vector :element-type 'character))
(defparameter *psc-accent-buffer* (make-extensible-vector :element-type 'character))

(defun put-sort-char (put-char full-vichar)
  (if (or (null full-vichar) (not (ascii-letter-p (caar full-vichar))))
      (progn
        (map () put-char *psc-letter-buffer*)
        (map () put-char *psc-accent-buffer*)
        (when full-vichar
          (funcall put-char (caar full-vichar)))
        (adjust-array *psc-letter-buffer* 0)
        (adjust-array *psc-accent-buffer* 0))
      (destructuring-bind ((char . num) . accent) full-vichar
        (vector-push-extend char *psc-letter-buffer*)
        (when (plusp (length (aref *vichar-to-viletter* (char-code char))))
          (vector-push-extend (digit-char num) *psc-letter-buffer*))
        (when (ascii-vowel-p char)
          (vector-push-extend (digit-char accent) *psc-accent-buffer*)))))

;; TODO: vietlex.com claims that lower-case comes before upper-case,
;; and that multiple non-letters are squashed into a zero.
(defun vi-sortable-string (string &optional (get-vichar 'get-unicode-char))
  (let ((i 0)
        (l (length string))
        (eos nil)
        (ss (make-string-output-stream))
        (letters (make-string-output-stream))
        (accents (make-string-output-stream))
        (unread nil))
    (labels
        ((peek ()
           (when (< i l) (char string i)))
         (next ()
           (incf i))
         (next-vichar ()
           (block nil
             (when unread
               (return (prog1 unread (setf unread nil))))
             (when (<= l i) (setf eos t))
             (unless eos
               (funcall get-vichar #'peek #'next))))
         (unread (x)
           (setf unread x)))
      (loop :until eos :do
        (loop :for full-vichar = (next-vichar)
          :for base-char = (caar full-vichar)
          :until (or (null full-vichar)
                     (and (ascii-letter-p base-char) (unread full-vichar))) :do
          (write-char base-char ss))
        (loop :for full-vichar = (next-vichar)
          :while (and full-vichar
                      (or (ascii-letter-p (caar full-vichar))
                          (progn (unread full-vichar) nil))) :do
          (destructuring-bind ((char . num) . accent) full-vichar
            (write-char char letters)
            (when num (write-char (digit-char num) letters))
            (when accent (write-char (digit-char accent) accents))))
        (write-string (get-output-stream-string letters) ss)
        (write-string (get-output-stream-string accents) ss))
      (get-output-stream-string ss))))

(defun vnsort (get-vichar sequence)
  (let ((sortable (map 'vector (lambda (x) (cons x (vi-sortable-string get-vichar x))) sequence)))
    (sort sortable 'string< :key 'cdr)
    (map 'list 'car sortable)))


;;;; TRANSLATE STRING
(defun transcode (full-vichar-getter full-vichar-putter
                  &key (input *standard-input*) (output *standard-output*))
  (with-input (input)
    (with-output (output)
      (labels
          ((peek () (peek-char nil input nil))
           (next () (read-char input nil))
           (put-char (char) (write-char char output)))
        (loop :for full-vichar = (funcall full-vichar-getter #'peek #'next)
          :for () = (funcall full-vichar-putter #'put-char full-vichar)
          :while full-vichar))))) ;; NB: the last call with nil allows for buffer flush

(defun unicode-from-viqr (&key input output)
  (transcode #'get-viqr-char #'put-unicode-char :input input :output output))

(defun viqr-from-unicode (&key input output)
  (transcode #'get-unicode-char #'put-viqr-char :input input :output output))

#|
############################### OPTION PROCESSING ###########################

my ($input_encoding, $ienc) ;
my ($output_encoding, $oenc) ;

my $get_vn_char ;
my $put_vn_char ;

my $do_it = \&usage ;

#(I hate those things)
sub Get_Options () {

  if ( $#ARGV < 0 ) { usage () ;}
  OPTION:
  while ($_=shift(@ARGV)) {
         if (/^(-i|--input)$/) {
             if (  ( $#ARGV >= 0 ) &&
	           ( $input_encoding = shift(@ARGV) ,
	           defined $vn_encodings{uc($input_encoding)} )  ) {
                    $ienc = $vn_encodings{uc($input_encoding)} ;
                    &{$init_tables[$ienc]}() ;
	            $get_vn_char = $get_vn_char[$ienc] ;
             } else {
                    errusage();
             };
    } elsif (/^(-o|--output)$/) {
             if (  ( $#ARGV >= 0 ) &&
	           ( $output_encoding = shift(@ARGV) ,
                   defined $vn_encodings{uc($output_encoding)} )  ) {
		    $oenc = $vn_encodings{uc($output_encoding)} ;
                    &{$init_tables[$oenc]}() ;
	            $put_vn_char = $put_vn_char[$oenc] ;
		    $do_it = \&translate ;
             } else {
                    errusage();
             };
    } elsif (/^(-s|--sort)$/) {
       ($output_encoding,$oenc) = (\&vnsort, $vn_encodings{"VNSORT"}) ;
       $do_it = \&do_sort;
    } elsif (/^(-V|--version)$/) {
       version();
    } elsif (/^(-[h\?]|--help)$/) {
       usage();
    } elsif (/^(-k|--keymap)$/) {
       $do_it = \&do_linux_keymap;
    } elsif (/^(-C|--compose)$/) {
       $do_it = \&do_X_Compose_table;
    } elsif (/^(-p|--psf)$/) {
       $do_it = \&do_linux_psf_table;
    } elsif (/^(--lisp-table)$/) {
       $do_it = \&do_lisp_table;
    } elsif (/--/) {
       last OPTION;
    } else {
       unshift @ARGV, $_ ;
       last OPTION;
    }
  }
#  if ( $ienc == $oenc ) {
#    die("Output encoding cannot be the same as input encoding!\n");
#  }
}

#################### FONT AND KEYMAP TABLES FOR LINUX CONSOLE ################
sub do_linux_psf_table {
  print <<"HEADER-END" ;
#
# This is a psf table for linux VISCII 1.1 console fonts.
# Automatically generated by <<vicode --psf>> (c) 1996-1999 Ð£ng-Vû Bân
# Use psfaddtable(1) to configure a font
#				ÐÄ ÐÄO BÁC
# You can freely distribute this software,
# as long as you make it preserve this notice,
# and make it clear what you possibly modified.
# You can freely use this software, as long as you're not a communist.
HEADER-END
  my $i ;
  for($i=0;$i<256;$i++) {
    printf "#x%02X U+%04X\n",$i,$viscii11_To_unicode[$i] ;
  }
}

sub lxquote {
  my $c = shift ;
  if ($c eq "'") { "\\'" } else { $c }
}

sub do_linux_keymap {
  my ($i) ;
  my @k1 =
( "A", "Z", "S", "E", "R", "I", "O", "L", "P", "U", "J", "Y",
  "a", "z", "s", "e", "r", "i", "o", "l", "p", "u", "j", "y",
  "D", "d", "D", "d" );
  my @k2 = ( " ", "\'", "`", "?", "~", "." );
#  my @k2 = ( " ", "b", "n", ",", ";", ":" );
#  my @k2 = ( " ", "b", "n", "m", ",", "." );
  print <<"HEADER-END"
#!/usr/bin/loadkeys
# This is a map of linux keyboard compose combinations for VISCII 1.1 input
# Automatically generated by <<vicode -k>> (c) 1996, 1997 Ð£ng-Vû Bân
# pipe it into loadkeys(1), or append it to your default keymap.
#				ÐÄ ÐÄO BÁC
# You can freely distribute this software,
# as long as you make it preserve this notice,
# and make it clear what you possibly modified.
# You can freely use this software, as long as you're not a communist.
HEADER-END
;
  for ($i=0;$i<=$#vn_table;$i++) {
     my $viqr=$vn_table[$i][0] ;
     my $viscii=chr($vn_table[$i][1]) ;
     my ($let,$acc) = viqr2let($viqr) ;
     my $c1 = $k1[$let] ;
     my $k1 = lxquote($c1) ;
     my $k2 = $acc ? $k2[$acc] : $k1 ;
     print "compose '$k1' '$k2' to '$viscii'\n" ;
     if (length($viqr)==2 && !$acc) {
       # Linux can only compose two characters :(   ]-:
       print "compose ",
             (map {"'".lxquote(chr($_))."' "} unpack("c*",$viqr)),
             "to '$viscii'\n" ;
     }
  }
}


sub do_lisp_table {
  my $i ;
  for($i=0;$i<=$#vn_table;$i++) {
    my $viqr = $vn_table[$i][0];
    my $viscii11 = $vn_table[$i][1];
    my $unicode = $viscii11_To_unicode[$viscii11] ;
    printf "    (%-5s #x%04X)\n","\"$viqr\"",$unicode ;
  }
}



########################### MISCELLANEOUS COMMANDS ###########################

sub version {
  print("$PROGRAM $VERSION\n");
  exit();
}

my $usage="Usage: $0 [options] [input-files]

Options summary:
short  long     parameter	meaning
==========================================================
-i     --input	<encoding>	specify input encoding
-o     --output	<encoding>	specify output encoding
-s     --sort			sort input
-k     --keymap			generate linux keymap hack
-C     --compose		generate X Compose keymap hack
-h     --help			print this help message
-V     --version		print version number
--     --      			end of options

Supported encodings:
s long			name
===================================================================
q viqr11 (viqr)    	VIQR 1.1 without quotes
v viscii11 (viscii)	VISCII 1.1 (RFC1456)
w vietword (vw)		VIETWORD 1.10
n tintuc (ndn)		NDN's amateur Mac fonts from Tin Tu+'c
s vnsort (sort)		VNSORT ready-to-sort format
0 viscii10 (v10)	VISCII 1.0
u unicode (uni)		Unicode (ISO-10646/UCS-2 vietnamese subset)
";

################################# SORT LINES #################################
sub do_sort {
  ### need an input encoding
  if (!$input_encoding) {
    die("cannot sort: input encoding not specified.");
  }

  ### Slurp input
  my @text = () ;
  while (<>) {
    push @text, $_;
  }

  ### Sort it
  my @sorted = vnsort ($get_vn_char,@text) ;

  ### Outputing it
  print @sorted ;

}


|#

(init-vichar-tables)
(init-viqr-tables)
(init-viscii11-tables)
(init-unicode-tables)
