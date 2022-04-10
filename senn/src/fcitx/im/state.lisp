(defpackage :senn.fcitx.im.state
  (:use :cl)
  (:export :katakana
           :katakana-input
           :make-katakana
           :selecting-from-predictions
           :selecting-from-predictions-predictions
           :selecting-from-predictions-current-index
           :selecting-from-predictions-current-input
           :selecting-from-predictions-move!
           :make-selecting-from-predictions))
(in-package :senn.fcitx.im.state)

(defstruct (katakana (:constructor %make-katakana))
  (input ""))

(defun make-katakana (&key input)
  (%make-katakana :input (senn.ja:hiragana->katakana input)))


(defstruct selecting-from-predictions predictions current-index)

(defun selecting-from-predictions-current-input (s)
  (nth (selecting-from-predictions-current-index s)
       (selecting-from-predictions-predictions s)))

(defun selecting-from-predictions-move! (s diff)
  (let ((new-index (+ (selecting-from-predictions-current-index s) diff)))
    (when (<= 0 new-index
              (1- (length (selecting-from-predictions-predictions s))))
      (setf (selecting-from-predictions-current-index s) new-index)))
  s)
