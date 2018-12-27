(defpackage :hachee.input-method.stateful
  (:use :cl)
  (:export :make-state
           :state-buffer
           :state-cursor-pos
           :transit-by-input)
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


(defstruct state
  (buffer "")
  (cursor-pos 0))

(defun transit-by-input (state code)
  (case code
    (65361 ;; left key
     (when (< 0 (state-cursor-pos state))
       (decf (state-cursor-pos state))))
    (65363 ;; right key
     (when (< (state-cursor-pos state)
              (1- (length (state-buffer state))))
       (incf (state-cursor-pos state))))
    (t
     (let ((new-buffer (romaji->hiragana (state-buffer state) code)))
       (setf (state-buffer state) new-buffer)
       (setf (state-cursor-pos state) (length new-buffer)))))
  state)



(assert (string= (romaji->hiragana "あk" (char-code #\a))
                 "あか"))
