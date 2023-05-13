(defpackage :senn.fcitx.stateful-ime
  (:use :cl)
  (:export :process-input
           :select-candidate
           :reset-im
           ; :reload-kkc
           :ime-kkc
           :make-ime))
(in-package :senn.fcitx.stateful-ime)

(defclass ime (senn.fcitx.im:ime)
  ((state
    :initarg :state
    :accessor ime-state)
   (kkc
    :initarg :kkc
    :reader ime-kkc)
   (predictor
    :initarg :predictor
    :initform nil
    :reader ime-predictor)))

(defmethod senn.fcitx.im:ime-kkc ((ime ime))
  (ime-kkc ime))

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

;; (defun reload-kkc (ime)
;;   (values))

;;;

(defun make-ime (&key kkc predictor)
  (make-instance 'ime
                 :state (senn.fcitx.im:make-initial-state)
                 :kkc kkc
                 :predictor predictor))
