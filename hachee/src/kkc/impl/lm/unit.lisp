(defpackage :hachee.kkc.impl.lm.unit
  (:use :cl)
  (:export :make-unit
           :unit-form
           :unit-pron
           :unit=
           :unit->key
           :unit->pron-units))
(in-package :hachee.kkc.impl.lm.unit)

;; word-pron pair model
;; unit = form/pron
(defun make-unit (&key form pron)
  (concatenate 'string form "/" pron))

(defun unit-form (unit)
  (first (cl-ppcre:split "/" unit)))

(defun unit-pron (unit)
  (second (cl-ppcre:split "/" unit)))

(defun unit= (unit1 unit2)
  (string= unit1 unit2))

(defun unit->key (unit)
  unit)

(defun unit->pron-units (unit)
  (loop for ch across (unit-pron unit)
        collect (make-unit :form (string ch)
                           :pron (string ch))))
