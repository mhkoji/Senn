(defpackage :hachee.kkc.convert
  (:use :cl)
  (:export :entry-origin
           :entry-form
           :entry-pron
           :entry-token
           :entry
           :dictionary-entry
           :unknown-word-entry))
(in-package :hachee.kkc.convert)

(defgeneric entry-origin (entry))

(defgeneric entry-unit (entry))

(defun entry-form (entry)
  (hachee.kkc.unit:unit-form (entry-unit entry)))

(defun entry-pron (entry)
  (hachee.kkc.unit:unit-pron (entry-unit entry)))

(defclass entry ()
  ((token
    :initarg :token
    :reader entry-token)))


(defclass dictionary-entry ()
  ((entry
    :initarg :entry)))

(defmethod entry-unit ((e dictionary-entry))
  (hachee.kkc.dictionary:entry-unit (slot-value e 'entry)))

(defmethod entry-origin ((e dictionary-entry))
  (hachee.kkc.dictionary:entry-origin (slot-value e 'entry)))


(defclass unknown-word-entry ()
  ((unit
    :initarg :unit
    :reader entry-unit)))

(defmethod entry-origin ((e unknown-unit-entry))
  :unknown-unit)
