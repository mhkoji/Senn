(defpackage :senn.im.segment
  (:use :cl)
  (:export :candidate
           :candidate-form
           :make-candidate

           :segment-candidates
           :segment-pron
           :segment-forms
           :segment-has-more-candidates-p
           :segment-current-form
           :segment-current-index
           :segment-shows-katakana-p
           :make-segment

           :append-forms!
           :try-move-cursor-pos!
           :try-set-cursor-pos!))
(in-package :senn.im.segment)

(defstruct candidate
  form)

(defstruct segment
  pron
  candidates
  current-index
  has-more-candidates-p
  shows-katakana-p)


(defun segment-forms (s)
  (mapcar #'candidate-form (segment-candidates s)))


(defun segment-current-form (s)
  (if (segment-shows-katakana-p s)
      (senn.ja:hiragana->katakana (segment-pron s))
      (candidate-form (elt (segment-candidates s)
                           (segment-current-index s)))))


(defun try-move-cursor-pos! (segment diff)
  (with-accessors ((forms segment-forms)
                   (current-index segment-current-index)) segment
    (setf current-index (mod (+ current-index diff)
                             (length forms))))
  segment)

(defun try-set-cursor-pos! (segment index)
  (with-accessors ((forms segment-forms)
                   (current-index segment-current-index)) segment
    (when (< -1 index (length forms))
      (setf current-index index)))
  segment)
