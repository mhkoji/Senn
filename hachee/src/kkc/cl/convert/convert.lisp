(defpackage :hachee.kkc.convert
  (:use :cl)
  (:export :make-entry
           :entry-origin
           :entry-form
           :entry-pron
           :entry-token
           :entry-unit
   
           :converter-begin-entry
           :converter-end-entry
           :converter-score-fn
           :converter-lookup-entries

           :execute))
(in-package :hachee.kkc.convert)

(defstruct entry
  unit token origin)

(defun entry-form (entry)
  (hachee.kkc.dictionary:unit-form (entry-unit entry)))

(defun entry-pron (entry)
  (hachee.kkc.dictionary:unit-pron (entry-unit entry)))

(defgeneric converter-begin-entry (converter))

(defgeneric converter-end-entry (converter))

(defgeneric converter-score-fn (converter))

(defgeneric converter-lookup-entries (converter pron))

(defun execute (converter pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi:execute pronunciation
   :begin-entry
   (converter-begin-entry converter)
   :end-entry
   (converter-end-entry converter)
   :score-fn
   (converter-score-fn converter)
   :list-entries-fn
   (lambda (sub-pron)
     (converter-lookup-entries converter sub-pron))
   :1st-boundary-index 1st-boundary-index))
