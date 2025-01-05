(defpackage :hachee.kkc.convert
  (:use :cl)
  (:export :entry-form
           :entry-pron
           :entry-origin

           :convert-begin-entry
           :convert-end-entry
           :convert-score-fn
           :convert-list-entries-fn
           :convert
           :2gram-convert
           :3gram-convert
           :execute))
(in-package :hachee.kkc.convert)

(defgeneric entry-form (e))
(defgeneric entry-pron (e))
(defgeneric entry-origin (e))

(defclass convert () ())

(defgeneric convert-begin-entry (convert))
(defgeneric convert-end-entry (convert))
(defgeneric convert-list-entries-fn (convert))
(defgeneric convert-score-fn (convert))
(defgeneric execute (convert pronunciation &key 1st-boundary-index))

(defclass 2gram-convert (convert) ())
(defclass 3gram-convert (convert) ())

(defmethod execute ((convert 2gram-convert) (pronunciation string)
                    &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi-2gram:execute pronunciation
   :begin-entry (convert-begin-entry convert)
   :end-entry (convert-end-entry convert)
   :score-fn (convert-score-fn convert)
   :list-entries-fn (convert-list-entries-fn convert)
   :1st-boundary-index 1st-boundary-index))

(defmethod execute ((convert 3gram-convert) (pronunciation string)
                    &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi-3gram:execute pronunciation
   :begin-entry (convert-begin-entry convert)
   :end-entry (convert-end-entry convert)
   :score-fn (convert-score-fn convert)
   :list-entries-fn (convert-list-entries-fn convert)
   :1st-boundary-index 1st-boundary-index))
