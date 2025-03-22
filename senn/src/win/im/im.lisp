(defpackage :senn.win.im
  (:use :cl)
  (:export :ime
           :ime-kkc
           :ime-predictor))
(in-package :senn.win.im)

(defclass ime (senn.im.inputting:mixin
               senn.im.converting:mixin)
  ((kkc
    :reader ime-kkc
    :initarg :kkc)
   (predictor
    :reader ime-predictor
    :initarg :predictor)))

(defmethod senn.im.inputting:ime-max-candidate-count ((ime ime))
  nil)

(defmethod senn.im.inputting:ime-predictor ((ime ime))
  (ime-predictor ime))

(defmethod senn.im.converting:ime-max-candidate-count ((ime ime))
  nil)

(defmethod senn.im.converting:ime-kkc ((ime ime))
  (ime-kkc ime))
