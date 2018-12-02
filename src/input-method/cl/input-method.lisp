(defpackage :hachee.input-method
  (:use :cl)
  (:export :romaji->hiragana)
  (:import-from :alexandria
                :if-let))
(in-package :hachee.input-method)

(defun romaji->hiragana (buffer code)
  (let ((buffer-len (length buffer))
        (string-char (string (code-char code))))
    (loop for i from (max 0 (- buffer-len 4)) to buffer-len
          for str-from-right
              = (concatenate 'string
                             (subseq buffer i buffer-len)
                             string-char)
          for romaji
              = (hachee.ja:romaji->hiragana str-from-right)
          when romaji
            do (return-from romaji->hiragana
                 (concatenate 'string (subseq buffer 0 i) romaji)))
    (concatenate 'string buffer string-char)))


(assert (string= (romaji->hiragana "あk" (char-code #\a))
                 "あか"))
