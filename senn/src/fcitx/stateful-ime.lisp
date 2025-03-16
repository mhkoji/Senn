(defpackage :senn.fcitx.stateful-ime
  (:use :cl)
  (:export :process-input
           :select-candidate
           :reset-im
           ;:reload-kkc
           :service
           :service-ime
           :service-kkc
           :make-service))
(in-package :senn.fcitx.stateful-ime)

(defclass service ()
  ((ime
    :initarg :ime
    :reader service-ime)
   (state
    :initarg :state
    :accessor service-state)))

(defun process-input (service key)
  (destructuring-bind (output state)
      (senn.fcitx.im:process-input
       (service-state service) (service-ime service) key)
    (when state
      (setf (service-state service) state))
    output))

(defun select-candidate (service index)
  (destructuring-bind (output state)
      (senn.fcitx.im:select-candidate
       (service-state service) (service-ime service) index)
    (when state
      (setf (service-state service) state))
    output))

(defun reset-im (service)
  (setf (service-state service)
        (senn.fcitx.im:make-initial-state service))
  (values))

;; (defun reload-kkc (ime)
;;   (values))

;;;

(defclass ime (senn.fcitx.im:ime-mixin)
  ((kkc
    :initarg :kkc
    :reader senn.fcitx.im:ime-kkc)
   (predictor
    :initarg :predictor
    :reader senn.fcitx.im:ime-predictor)))

(defun make-service (&key kkc predictor)
  (let ((ime (make-instance 'ime
                            :kkc kkc
                            :predictor predictor)))
    (let ((state (senn.fcitx.im:make-initial-state ime)))
      (make-instance 'service :ime ime :state state))))

(defun service-kkc (service)
  (senn.fcitx.im:ime-kkc (service-ime service)))
