(defpackage :senn.im
  (:use :cl)
  (:export :ime
           :convert
           :lookup-forms))
(in-package :senn.im)

(defclass ime () ())

(defgeneric convert (ime pron))

(defgeneric lookup-forms (ime pron))
