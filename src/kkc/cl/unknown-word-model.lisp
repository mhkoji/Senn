(defpackage :hachee.kkc.unknown-word-model
  (:use :cl)
  (:export :probability))
(in-package :hachee.kkc.unknown-word-model)

(defgeneric probability (model word-string))

(defmethod probability ((model t) (word-string t))
  1.0d-10)
