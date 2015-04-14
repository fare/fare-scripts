(defpackage :fare-scripts/rescript
  (:use :common-lisp :uiop)
  (:export #:superscriptize #:upsidedown #:leftright))

(in-package :fare-scripts/rescript)

(defun make-script-table (original translated &key reversible)
  (loop :with h = (make-hash-table :test 'equal)
        :for x :across original
        :for y :across translated
        :do (setf (gethash x h) y)
            (when reversible
              (setf (gethash y h) x))
        :finally (return h)))

(defmacro define-script-translation (name original translated
                                     &key reversible nest)
  (let ((table (intern (format nil "*~A-~A*" name 'table)))
        (process-char (intern (format nil "~A-~A" name 'character))))
    `(progn
       (defparameter ,table
         (make-script-table ,original ,translated :reversible ,reversible))
       (defun ,process-char (c)
         (or (gethash c ,table)
             (error "Cannot ~S ~S" ',process-char c)))
       (defun ,name (s)
         (nest
          ,@(when nest (list nest))
          (map 'string ',process-char s))))))

(define-script-translation superscriptize
  " 0123456789+-=()abcdefghijklmnoprstuvwxyzABDEGHIJKLMNOPRTUVWαβγδεθιΦφχ"
  " ⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁⱽᵂᵅᵝᵞᵟᵋᶿᶥᶲᵠᵡ")

(define-script-translation upsidedown
  " zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA0987654321&_?!\"'.,;"
  " zʎxʍʌnʇsɹbdouɯlʞɾıɥɓɟǝpɔqɐZ⅄XMΛ∩⊥SᴚԾԀONW⅂⋊ſIH⅁ℲƎᗡƆ𐐒∀068ㄥ9ގㄣƐᄅ⇂⅋‾¿¡„,˙'؛"
  :reversible t :nest (reverse))

(define-script-translation leftright
  " 018!\"'.:-_+=|()[]{}<>/\\´`ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  " 018!\"'.:-_+=|)(][}{><\\/`´ᗅᗺƆᗡƎꟻᎮHIႱ⋊⅃MͶOꟼỌЯꙄTUVWXYƸɒdɔbɘᎸǫʜiꞁʞ|mᴎoqpɿꙅƚuvwxʏƹ"
  :reversible t :nest (reverse))

