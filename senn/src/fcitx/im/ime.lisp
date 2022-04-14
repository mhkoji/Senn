(defpackage :senn.fcitx.im.ime
  (:use :cl)
  (:export :ime
           :ime-max-candidate-count
           :ime-kkc
           :ime-predictor))
(in-package :senn.fcitx.im.ime)

(defclass ime (senn.im.inputting:ime
               senn.im.converting:ime)
  ())

(defgeneric ime-max-candidate-count (ime)
  (:method ((ime ime))
    nil))

(defgeneric ime-kkc (ime))

(defgeneric ime-predictor (ime)
  (:method ((ime ime))
    nil))
;;;

(defmethod senn.im.inputting:ime-max-candidate-count ((ime ime))
  (ime-max-candidate-count ime))

(defmethod senn.im.inputting:ime-predictor ((ime ime))
  (ime-predictor ime))

(defmethod senn.im.converting:ime-max-candidate-count ((ime ime))
  (ime-max-candidate-count ime))

(defmethod senn.im.converting:ime-kkc ((ime ime))
  (ime-kkc ime))
