;;; -*- mode: Lisp; coding: utf-8 -*-
;;; Testing vicode
;;;
(defpackage :fare-scripts/vicode-test
  (:use :common-lisp :uiop :fare-scripts/vicode :hu.dwim.stefil)
  (:export #:test-suite))
(in-package :fare-scripts/vicode-test)

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing fare-scripts/vicode"))

(defparameter *strings*
  '("a"
    "AĂÂBCDĐEÊGHIKLMNOÔƠPQRSTUƯVXY"
    "an"
    "an ninh"
    "an toàn"
    "anh"
    "ảnh"
    "ánh sáng"
    "ă"
    "ăn"
    "â" "b" "c" "d" "e" "ê"
    "f" "g" "h"
    "hello world"
    "i" "j" "k" "l" "m"
    "ma" "mà" "mả" "mã" "má" "mạ"
    "n" "o" "ô" "ơ" "p" "q" "r" "s" "t"
    "tôi học tiếng Việt"
    "u" "ư" "v" "w" "x" "y" "z"
    ))

(deftest sort-test-1 ()
  (is (equal *strings* (sort (copy-list *strings*) 'string<
                             :key (lambda (x) (vi-sortable-string (string-downcase x)))))))

(deftest viqr-test-1 ()
  (loop :for (viqr unicode) :in
    '(("to^i ho.c tie^'ng Vie^.t" "tôi học tiếng Việt")
      ("ba` ba be'o ba'n ba'nh be`o ba'nh bo` ba'nh bao bu+`a ba~i be^n bo+` bie^?n bi. ba('t bo? bo't ba bo^'n ba^.n."
       "bà ba béo bán bánh bèo bánh bò bánh bao bừa bãi bên bờ biển bị bắt bỏ bót ba bốn bận.")) :do
    (is (equal viqr (viqr-from-unicode :input unicode)))
    (is (equal unicode (unicode-from-viqr :input viqr)))))

