(defpackage :senn.fcitx.im.immutable
  (:use :cl)
  (:export :process-input
           :select-candidate
           :make-initial-state
           :make-output
           :ime-mixin
           :ime-max-candidate-count
           :ime-kkc
           :ime-predictor))
(in-package :senn.fcitx.im.immutable)

(defgeneric process-input (state key ime))

(defgeneric select-candidate (state key ime))

(defgeneric make-initial-state (ime))

(defun make-output (view)
  (let ((consumed-p (and view t)))
    (list consumed-p view)))

;;;

(defclass ime-mixin (senn.fcitx.im.immutable.state.inputting:mixin
                     senn.fcitx.im.immutable.state.converting:mixin)
  ())

(defgeneric ime-max-candidate-count (ime)
  (:method ((ime ime-mixin))
    nil))

(defgeneric ime-kkc (ime))

(defgeneric ime-predictor (ime)
  (:method ((ime ime-mixin))
    nil))

(defmethod make-initial-state ((ime ime-mixin))
  (senn.fcitx.im.immutable.state.inputting:make-state))

(defmethod senn.fcitx.im.immutable.state.inputting:ime-max-candidate-count
    ((ime ime-mixin))
  (ime-max-candidate-count ime))

(defmethod senn.fcitx.im.immutable.state.inputting:ime-predictor
    ((ime ime-mixin))
  (ime-predictor ime))

(defmethod senn.fcitx.im.immutable.state.converting:ime-max-candidate-count
    ((ime ime-mixin))
  (ime-max-candidate-count ime))

(defmethod senn.fcitx.im.immutable.state.converting:ime-kkc
    ((ime ime-mixin))
  (ime-kkc ime))
