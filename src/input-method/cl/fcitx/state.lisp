(defpackage :hachee.input-method.fcitx.state
  (:use :cl)
  (:export :make-state
           :state-type
           :state-buffer
           :state-cursor-pos
           :transit-by-input)
  (:import-from :alexandria
                :if-let))
(in-package :hachee.input-method.fcitx.state)

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
  type
  (buffer "")
  (cursor-pos 0))

(defun transit-by-input (state code)
  (when (= code 65293) ;; Enter key
    (setf (state-type state) :committed)
    (return-from transit-by-input state))

  (when (eql (state-type state) :committed)
    (setq state (make-state)))

  (case code
    (65361 ;; Left key
     (when (< 0 (state-cursor-pos state))
       (decf (state-cursor-pos state)))
     state)
    (65363 ;; Right key
     (when (< (state-cursor-pos state)
              (1- (length (state-buffer state))))
       (incf (state-cursor-pos state)))
     state)
    (t
     (let ((new-buffer (romaji->hiragana (state-buffer state) code)))
       (setf (state-buffer state) new-buffer)
       (setf (state-cursor-pos state) (length new-buffer))
       state))))


(assert (string= (romaji->hiragana "あk" (char-code #\a))
                 "あか"))
