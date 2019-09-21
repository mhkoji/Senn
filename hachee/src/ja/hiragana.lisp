(in-package :hachee.ja)

(defun htok (c)
  (let ((code (char-code c))
        (diff #.(- (char-code #\ア) (char-code #\あ))))
    (if (<= #.(char-code #\ぁ) code #.(char-code #\ゖ))
        (code-char (+ code diff))
        c)))

(defun hiragana->katakana (string)
  (map 'string #'htok string))
