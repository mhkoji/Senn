(defpackage :senn.win.im
  (:use :cl)
  (:export :ime
           :ime-kkc
           :ime-predictor))
(in-package :senn.win.im)

(defclass ime () ())
(defgeneric ime-kkc (ime))
(defgeneric ime-predictor (ime)
  (:method ((ime ime))
    nil))
