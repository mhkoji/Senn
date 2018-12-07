(defpackage :hachee.input-method.stateful
  (:use :cl)
  (:export :transit-by-input)
  (:import-from :alexandria
                :if-let))
(in-package :hachee.input-method.stateful)

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


(defun transit-by-input (buffer code)
  (let ((new-buffer (romaji->hiragana buffer code)))
    (list new-buffer new-buffer)))


(assert (string= (romaji->hiragana "あk" (char-code #\a))
                 "あか"))
