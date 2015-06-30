(uiop:define-package :fare-scripts/random
  (:use :cl :uiop :fare-utils :optima :optima.ppcre :binascii :cl-scripting)
  (:export
   ;; #:randomize #:random-bytes #:urandom
   ;; #:*diceware* #:*diceware-words*
   ;; #:dice #:roll-index #:roll-string #:get-diceware-words #:ensure-diceware-words
   ;; #:diceware-word
   #:diceware-phrase #:genpasswd #:genresa
   ))

(in-package :fare-scripts/random)

(defun randomize ()
  (setf *random-state* (make-random-state t)))

(register-image-restore-hook 'randomize)


#|(defun ensure-prng ()
  (unless crypto:*prng* (setf crypto:*prng* (crypto:make-prng :fortuna))))|#

(defun random-bytes (n)
  (let ((x (make-array
            (list n)
            :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-open-file (s "/dev/urandom"
                       :direction :input :element-type '(unsigned-byte 8))
      (read-sequence x s))
    x))

(defun urandom (n)
  ;; in case you don't trust your implementation's RANDOM, you can use URANDOM.
  (let* ((n-bytes (ceiling (+ 64 (log n 2)) 8)) ;; get 64 extra bits everytime, minimizing the mismatch between n and 2**m
         (bytes (random-bytes n-bytes))
         (big-n (reduce (lambda (x y) (mod (+ (ash x 8) y) n)) bytes)))
    (mod big-n n)))

(defparameter *diceware*
  '(:file #p"/home/tunes/src/security/diceware-fr/diceware-fr-5-jets.txt" :n-dice 5))

(defvar *diceware-words* ())

(defun dice (&optional (n 1) (sides 6))
  (loop :repeat n :sum (1+ (random sides))))

(defun roll-index (&optional x (n-dice (getf *diceware* :n-dice)))
  (etypecase x
    (null
     (random (expt 6 n-dice)))
    ((integer * 0)
     (assert (< (- (expt 6 n-dice)) x))
     (- x))
    (integer
     (roll-index (princ-to-string x)))
    (list
     (assert (= n-dice (length x)))
     (loop :for d :in (reverse x)
           :for m = 1 :then (* 6 m)
           :do (assert (typep d '(integer 1 6)))
           :sum (* m (1- d))))
    (string
     (assert (= n-dice (length x)))
     (roll-index
      (loop :repeat n-dice :for c :across x :for d = (digit-char-p c)
            :do (assert (typep d '(integer 1 6)))
            :collect d)))))

(defun roll-string (&optional x (n-dice (getf *diceware* :n-dice)))
  (map 'string (lambda (c) (code-char (1+ (char-code c))))
       (format nil "~6,v,'0R" n-dice (roll-index x n-dice))))

(defun get-diceware-words ()
  (destructuring-bind (&key file n-dice) *diceware*
    (let* ((limit (expt 6 n-dice))
           (words (make-array limit)))
      (with-input-file (w file)
        (loop :for i :from 0
              :for l = (read-line w nil)
              :while l
              :do (match l
                    ((ppcre (strcat "^([1-6]+) ([!-~]+)$") n x)
                     (assert (= i (roll-index n n-dice)))
                     (setf (aref words i) x))
                    (_ (error "invalid line in diceware file: ~A" l)))
              :finally (assert (= i limit))))
      words)))

(defun ensure-diceware-words ()
  (destructuring-bind (&key file n-dice) *diceware*
    (declare (ignore file))
    (unless (typep *diceware-words* `(vector * ,(expt 6 n-dice)))
      (setf *diceware-words* (get-diceware-words))))
  (values))


(defun diceware-word (&optional roll)
  (ensure-diceware-words)
  (aref *diceware-words* (roll-index roll)))

(exporting-definitions

(defun diceware-phrase (&optional (n-words 3) &rest more-rolls)
  (let ((rolls
          (if (or more-rolls (not (typep n-words '(integer 1 99))))
              (cons n-words more-rolls)
              (loop :repeat n-words :collect (roll-string)))))
    (join-strings (mapcar 'diceware-word rolls) :separator " ")))

(defun genpasswd ()
  (binascii:encode (random-bytes 32) :base64))

(defun genresa ()
  (format nil "~36R" (random (expt 36 6))))

);exporting-definitions

(register-commands :fare-scripts/random)
