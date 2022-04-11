(defpackage :senn.fcitx.stateful-ime
  (:use :cl)
  (:export :process-input
           :select-candidate
           :reset-im
           ; :reload-kkc
           :ime-kkc-store
           :make-ime))
(in-package :senn.fcitx.stateful-ime)

(defclass ime (senn.fcitx.im:ime)
  ((state
    :initarg :state
    :accessor ime-state)
   (kkc-store
    :initarg :kkc-store
    :reader ime-kkc-store)
   (predictor
    :initarg :predictor
    :initform nil
    :reader ime-predictor)))

(defmethod senn.fcitx.im:ime-kkc ((ime ime))
  (senn.im.kkc-store:get-kkc (ime-kkc-store ime)))

(defmethod senn.fcitx.im:ime-predictor ((ime ime))
  (ime-predictor ime))

;;;

(defun process-input (ime key)
  (destructuring-bind (resp state)
      (senn.fcitx.im:process-input ime (ime-state ime) key)
    (when state
      (setf (ime-state ime) state))
    resp))

(defun select-candidate (ime index)
  (destructuring-bind (resp state)
      (senn.fcitx.im:select-candidate (ime-state ime) index)
    (when state
      (setf (ime-state ime) state))
    resp))

(defun reset-im (ime)
  (setf (ime-state ime) (senn.fcitx.im:make-initial-state))
  (values))

(defun reload-kkc (ime)
  (senn.im.kkc-store:reload (slot-value ime 'kkc-store))
  (values))

;;;

(defun make-ime (&key kkc-store predictor)
  (make-instance 'ime
                 :state (senn.fcitx.im:make-initial-state)
                 :kkc-store kkc-store
                 :predictor predictor))
