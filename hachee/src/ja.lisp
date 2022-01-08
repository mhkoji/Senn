(defpackage :hachee.ja
  (:use :cl)
  (:export :hiragana->katakana))
(in-package :hachee.ja)

(defun hiragana->katakana (string)
  (labels ((htok (c)
             (let ((code (char-code c))
                   (diff #.(- (char-code #\ア) (char-code #\あ))))
               (if (<= #.(char-code #\ぁ) code #.(char-code #\ゖ))
                   (code-char (+ code diff))
                   c))))
    (map 'string #'htok string)))
