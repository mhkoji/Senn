(defpackage :hachee.language-model.corpus
  (:use :cl)
  (:export :sentence
           :sentence-tokens
           :make-sentence
           :corpus
           :corpus-sentence-list
           :make-corpus))
(in-package :hachee.language-model.corpus)

(defstruct sentence tokens)

(defstruct corpus sentence-list)
