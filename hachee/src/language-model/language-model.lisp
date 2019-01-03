(defpackage :hachee.language-model
  (:use :cl)
  (:export :sentence
           :sentence-tokens
           :make-sentence))
(in-package :hachee.language-model)

(defstruct sentence tokens)
