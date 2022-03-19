(defpackage :hachee.kkc.lookup
  (:use :cl)
  (:export :item-form
           :item-origin
           :execute))
(in-package :hachee.kkc.lookup)

(defgeneric item-form (item))
(defgeneric item-origin (item))

(defgeneric execute (lookup pronunciation &key prev next))
