(defpackage :hachee.kkc.models.unknown-word
  (:use :cl)
  (:export :probability))
(in-package :hachee.kkc.models.unknown-word)

(defgeneric probability (model word-string))

(defmethod probability ((model t) (word-string t))
  1.0d-10)
