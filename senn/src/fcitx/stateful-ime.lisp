(defpackage :senn.fcitx.stateful-ime
  (:use :cl)
  (:export :process-input
           :select-candidate
           :reset-im
           ;:reload-kkc
           :service
           :service-ime))
(in-package :senn.fcitx.stateful-ime)

(defclass service ()
  ((ime
    :initarg :ime
    :reader service-ime)
   (state
    :initarg :state
    :accessor service-state)))

(defmethod initialize-instance :after ((service service) &key)
  (setf (service-state service)
        (senn.fcitx.ime:make-initial-state (service-ime service))))

(defun process-input (service key)
  (destructuring-bind (output state)
      (senn.fcitx.ime:process-input
       (service-state service) (service-ime service) key)
    (when state
      (setf (service-state service) state))
    output))

(defun select-candidate (service index)
  (destructuring-bind (output state)
      (senn.fcitx.ime:select-candidate
       (service-state service) (service-ime service) index)
    (when state
      (setf (service-state service) state))
    output))

(defun reset-im (service)
  (setf (service-state service)
        (senn.fcitx.ime:make-initial-state service))
  (values))

;; (defun reload-kkc (ime)
;;   (values))
