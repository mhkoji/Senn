(defpackage :senn.ibus.stateful-ime
  (:use :cl)
  (:export :process-input
           :toggle-input-mode
           :select-candidate
           :reset-im
           :service
           :service-ime
           :service-kkc
           :make-service))
(in-package :senn.ibus.stateful-ime)

(defstruct state
  input-mode
  fcitx-state)

(defun make-initial-state ()
  (make-state
   :input-mode :direct
   :fcitx-state nil))

(defclass service ()
  ((ime
    :initarg :ime
    :reader service-ime)
   (state
    :initarg :state
    :initform (make-initial-state)
    :accessor service-state)))

(defun process-input (service key)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (service-state service)
    (if (eq input-mode :hiragana)
        (destructuring-bind (output state)
            (senn.fcitx.im:process-input
             fcitx-state (service-ime service) key)
          (when state
            (setf fcitx-state state))
          output)
        (senn.fcitx.im:make-output nil))))

(defun toggle-input-mode (service)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (service-state service)
    (ecase input-mode
      (:hiragana
       (setf input-mode :direct)
       (setf fcitx-state nil))
      (:direct
       (setf input-mode :hiragana)
       (setf fcitx-state (senn.fcitx.im:make-initial-state
                          (service-ime service)))))
    (format nil "~A" input-mode)))

(defun select-candidate (service index)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (service-state service)
    (if (eq input-mode :hiragana)
        (destructuring-bind (output state)
            (senn.fcitx.im:select-candidate
             fcitx-state (service-ime service) index)
          (when state
            (setf fcitx-state state))
          output)
        (senn.fcitx.im:make-output nil))))

(defun reset-im (service)
  (setf (service-state service) (make-initial-state))
  (values))

;;;

(defclass ime (senn.fcitx.im:ime-mixin)
  ((kkc
    :initarg :kkc
    :reader senn.fcitx.im:ime-kkc)))

(defmethod senn.fcitx.im:ime-max-candidate-count ((ime ime))
  ;; An error occurs if the candidate count >= 16.
  15)

(defun make-service (&key kkc)
  (let ((ime (make-instance 'ime :kkc kkc)))
    (make-instance 'service :ime ime)))

(defun service-kkc (service)
  (senn.fcitx.im:ime-kkc (service-ime service)))
