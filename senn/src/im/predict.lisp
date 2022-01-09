(defpackage :senn.im.predict
  (:use :cl)
  (:export :execute))
(in-package :senn.im.predict)

(defgeneric execute (predictor string)
  (:method-combination append))
