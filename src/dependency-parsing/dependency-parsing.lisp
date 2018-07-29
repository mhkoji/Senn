(defpackage :hachee.dependency-parsing
  (:use :cl))
(in-package :hachee.dependency-parsing)

(defstruct row form lemma cpostag postag feats head deprel cluster)
(export '(make-row
          row
          row-form
          row-lemma
          row-cpostag
          row-postag
          row-feats
          row-head
          row-deprel
          row-cluster))

(defstruct sentence rows id)
(export '(make-sentence
          sentence
          sentence-rows
          sentence-id))

(defun sentence-length (sentence)
  (length (sentence-rows sentence)))
(export '(sentence-length))

(defparameter +root+
  (make-row :form   "ROOT"
            :lemma  "ROOT"
            :postag "ROOT"
            :head   -1
            :deprel "ROOT"
            :cluster 0))
(export '(+root+))
