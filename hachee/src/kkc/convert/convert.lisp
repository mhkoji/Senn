(defpackage :hachee.kkc.convert
  (:use :cl)
  (:export :entry-form
           :entry-pron
           :entry-origin

           :convert-begin-entry
           :convert-end-entry
           :convert-score-fn
           :convert-list-entries-fn
           :execute

           :convert-viterbi-2nd-score-fn
           :viterbi-2nd))
(in-package :hachee.kkc.convert)

(defgeneric entry-form (e))
(defgeneric entry-pron (e))
(defgeneric entry-origin (e))

(defgeneric convert-begin-entry (mixin))
(defgeneric convert-end-entry (mixin))
(defgeneric convert-score-fn (mixin))
(defgeneric convert-list-entries-fn (mixin))

(defgeneric convert-viterbi-2nd-score-fn (mixin))

(defun execute (convert pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi:execute pronunciation
   :begin-entry (convert-begin-entry convert)
   :end-entry (convert-end-entry convert)
   :score-fn (convert-score-fn convert)
   :list-entries-fn (convert-list-entries-fn convert)
   :1st-boundary-index 1st-boundary-index))

(defun viterbi-2nd (convert pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi-2nd:execute pronunciation
   :begin-entry (convert-begin-entry convert)
   :end-entry (convert-end-entry convert)
   :score-fn (convert-viterbi-2nd-score-fn convert)
   :list-entries-fn (convert-list-entries-fn convert)
   :1st-boundary-index 1st-boundary-index))
