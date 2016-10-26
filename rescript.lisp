;;; -*- mode: Lisp; coding: utf-8 -*-
;; ᴸᴀᵀᴇᵡ

(defpackage :fare-scripts/rescript
  (:use :common-lisp :uiop)
  (:export #:superscriptize #:subscriptize #:upsidedown #:leftright #:mathbb #:smallcaps))

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

(define-script-translation subscriptize
  " 0123456789+-=()aehijklmnoprstuvxβγρφχəا"
  " ₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓᵦᵧᵨᵩᵪₔٖ")

(define-script-translation upsidedown
  " zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA0987654321&_?!\"'.,;"
  " zʎxʍʌnʇsɹbdouɯlʞɾıɥɓɟǝpɔqɐZ⅄XMΛ∩⊥SᴚԾԀONW⅂⋊ſIH⅁ℲƎᗡƆ𐐒∀068ㄥ9ގㄣƐᄅ⇂⅋‾¿¡„,˙'؛"
  :reversible t :nest (reverse))

(define-script-translation leftright
  " 018!\"'.:-_+=|()[]{}<>/\\´`ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  " 018!\"'.:-_+=|)(][}{><\\/`´ᗅᗺƆᗡƎꟻᎮHIႱ⋊⅃MͶOꟼỌЯꙄTUVWXYƸɒdɔbɘᎸǫʜiꞁʞ|mᴎoqpɿꙅƚuvwxʏƹ"
  :reversible t :nest (reverse))

#-allegro ;; Allegro gets confused, possibly because of codepoints > 65535 ?
(define-script-translation mathbb
  " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  " 𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡")

(define-script-translation smallcaps
  " ABCDEFGHIJKLMNOPRSTUVWYZ"
  " ᴀʙᴄᴅᴇꜰɢʜɪᴊᴋʟᴍɴᴏᴘʀsᴛᴜᴠᴡʏᴢ")

(defun search-char-name (subname)
  (loop
    :for i :from 0 :below char-code-limit
    :for c = (ignore-errors (code-char i))
    :for n = (and c (char-name c))
    :when (and n (search subname n))
    :do (format t "~D ~C ~A~%" i c n)))
