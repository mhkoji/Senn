(defpackage :hachee.kkc.convert
  (:use :cl)
  (:export :entry-origin
           :entry-form
           :entry-pron
           :entry-token
           :entry-unit
           :entry
           :make-entry))
(in-package :hachee.kkc.convert)

(defstruct entry
  unit token origin)

(defun entry-form (entry)
  (hachee.kkc.dictionary:unit-form (entry-unit entry)))

(defun entry-pron (entry)
  (hachee.kkc.dictionary:unit-pron (entry-unit entry)))
