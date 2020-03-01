(defpackage :senn.im.predict
  (:use :cl)
  (:export :prefix-predictor))
(in-package :senn.im.predict)

(defclass prefix-predictor ()
  ((dictionary
    :initarg :dictionary
    :reader prefix-predictor-dictionary)))

(defmethod senn.im:predict append ((predictor prefix-predictor)
                                   (string string))
  (let ((dict (prefix-predictor-dictionary predictor)))
    (senn.prefix-dictionary:lookup dict string)))
