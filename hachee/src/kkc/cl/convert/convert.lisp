(defpackage :hachee.kkc.convert
  (:use :cl)
  (:export :make-entry
           :entry-origin
           :entry-form
           :entry-pron
           :entry-token
           :entry-unit

           :convert-begin-entry
           :convert-end-entry
           :convert-score-fn
           :convert-list-entries-fn
           :execute))
(in-package :hachee.kkc.convert)

(defstruct entry
  unit token origin)

(defun entry-form (entry)
  (hachee.kkc.dictionary:unit-form (entry-unit entry)))

(defun entry-pron (entry)
  (hachee.kkc.dictionary:unit-pron (entry-unit entry)))

(defgeneric convert-begin-entry (mixin))
(defgeneric convert-end-entry (mixin))
(defgeneric convert-score-fn (mixin))
(defgeneric convert-list-entries-fn (mixin))

(defun execute (convert pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi:execute pronunciation
   :begin-entry (convert-begin-entry convert)
   :end-entry (convert-end-entry convert)
   :score-fn (convert-score-fn convert)
   :list-entries-fn (convert-list-entries-fn convert)
   :1st-boundary-index 1st-boundary-index))
