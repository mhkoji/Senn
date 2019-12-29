;;; Under construction
(defpackage :senn.im2
  (:use :cl)
  (:export :ime
           :chars
           :input-char))
(in-package :senn.im2)

(defclass ime () ())

(defgeneric chars (state))

(defgeneric input-char (ime state char))
