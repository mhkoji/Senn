(defpackage :senn.fcitx.im.view
  (:use :cl)
  (:export :editing
           :editing-cursor-pos
           :editing-input
           :editing-predictions
           :editing-prediction-index
           :editing-committed-input
           :make-editing
           :converting-cursor-form
           :converting-cursor-form-candidates
           :converting-cursor-form-candidate-index
           :make-converting-cursor-form
           :converting
           :converting-forms
           :converting-cursor-form-index
           :make-converting))
(in-package :senn.fcitx.im.view)

(defstruct editing
  cursor-pos
  input
  predictions
  prediction-index
  committed-input)


(defstruct converting-cursor-form
  candidates
  candidate-index)

(defstruct converting
  forms
  cursor-form-index
  cursor-form)
