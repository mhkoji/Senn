(defpackage :senn.fcitx.im.immutable
  (:use :cl)
  (:export :ime
           :ime-max-candidate-count
           :ime-kkc
           :ime-predictor
           :process-input
           :select-candidate
           :make-initial-state))
(in-package :senn.fcitx.im.immutable)

(defclass ime (senn.fcitx.im.immutable.process-input:mixin)
  ())

(defgeneric ime-max-candidate-count (ime)
  (:method ((ime ime))
    nil))

(defgeneric ime-kkc (ime))

(defgeneric ime-predictor (ime)
  (:method ((ime ime))
    nil))

(defun process-input (ime state key)
  (senn.fcitx.im.immutable.process-input:execute state key ime))

(defun select-candidate (state index)
  (senn.fcitx.im.immutable.select-candidate:execute state index))

(defun make-initial-state ()
  (senn.fcitx.im.state.inputting:make-state))

;;;

(defmethod senn.fcitx.im.state.inputting:ime-max-candidate-count ((ime ime))
  (ime-max-candidate-count ime))

(defmethod senn.fcitx.im.state.inputting:ime-predictor ((ime ime))
  (ime-predictor ime))

(defmethod senn.fcitx.im.state.converting:ime-max-candidate-count ((ime ime))
  (ime-max-candidate-count ime))

(defmethod senn.fcitx.im.state.converting:ime-kkc ((ime ime))
  (ime-kkc ime))
