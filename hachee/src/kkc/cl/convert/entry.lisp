(defpackage :hachee.kkc.convert
  (:use :cl)
  (:export :entry-origin
           :entry-form
           :entry-pron
           :entry-token
           :entry-unit
           :entry
           :dictionary-entry
           :non-dictionary-entry))
(in-package :hachee.kkc.convert)

(defgeneric entry-origin (entry))

(defgeneric entry-unit (entry))

(defun entry-form (entry)
  (hachee.kkc.dictionary:unit-form (entry-unit entry)))

(defun entry-pron (entry)
  (hachee.kkc.dictionary:unit-pron (entry-unit entry)))

(defclass entry ()
  ((token
    :initarg :token
    :reader entry-token)))

(defmethod print-object ((entry entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "~A [~A, ~A]"
            (entry-form entry)
            (entry-token entry)
            (entry-origin entry))))

(defclass dictionary-entry (entry)
  ((entry
    :initarg :entry)))

(defmethod entry-unit ((e dictionary-entry))
  (hachee.kkc.dictionary:entry-unit (slot-value e 'entry)))

(defmethod entry-origin ((e dictionary-entry))
  (hachee.kkc.dictionary:entry-origin (slot-value e 'entry)))


(defclass non-dictionary-entry (entry)
  ((unit
    :initarg :unit
    :reader entry-unit)
   (origin
    :initarg :origin
    :reader entry-origin)))
