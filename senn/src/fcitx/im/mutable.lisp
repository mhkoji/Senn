(defpackage :senn.fcitx.im.mutable
  (:use :cl)
  (:export :process-input
           :select-candidate
           :reset-im
           :get-ime
           :get-kkc
           ; :reload-kkc
           :make-ime))
(in-package :senn.fcitx.im.mutable)

(defclass ime (senn.fcitx.im.immutable:ime-mixin)
  ((kkc
    :initarg :kkc
    :reader senn.fcitx.im.immutable:ime-kkc)
   (predictor
    :initarg :predictor
    :reader senn.fcitx.im.immutable:ime-predictor)))

(defclass stateful-ime ()
  ((ime
    :initarg :ime
    :reader get-ime)
   (state
    :initarg :state
    :accessor get-state)))

(defun make-ime (&key kkc predictor)
  (let ((ime (make-instance 'ime
              :kkc kkc
              :predictor (or predictor nil))))
    (let ((state (senn.fcitx.im.immutable:make-initial-state ime)))
      (make-instance 'stateful-ime :ime ime :state state))))

(defun get-kkc (stateful-ime)
  (slot-value (get-ime stateful-ime) 'kkc))

;;;

(defun process-input (stateful-ime key)
  (destructuring-bind (output state)
      (senn.fcitx.im.immutable:process-input
       (get-state stateful-ime) key (get-ime stateful-ime))
    (when state
      (setf (get-state stateful-ime) state))
    output))

(defun select-candidate (stateful-ime index)
  (destructuring-bind (output state)
      (senn.fcitx.im.immutable:select-candidate
       (get-state stateful-ime) index (get-ime stateful-ime))
    (when state
      (setf (get-state stateful-ime) state))
    output))

(defun reset-im (stateful-ime)
  (setf (get-state stateful-ime)
        (senn.fcitx.im.immutable:make-initial-state (get-ime stateful-ime)))
  (values))

;; (defun reload-kkc (ime)
;;   (values))
