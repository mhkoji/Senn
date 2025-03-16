(defpackage :senn.ibus.stateful-ime
  (:use :cl)
  (:export :process-input
           :toggle-input-mode
           :select-candidate
           :reset-im
           :service
           :service-ime))
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
            (senn.fcitx.ime:process-input
             fcitx-state (service-ime service) key)
          (when state
            (setf fcitx-state state))
          output)
        (senn.fcitx.ime:make-output nil))))

(defun toggle-input-mode (service)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (service-state service)
    (ecase input-mode
      (:hiragana
       (setf input-mode :direct)
       (setf fcitx-state nil))
      (:direct
       (setf input-mode :hiragana)
       (setf fcitx-state (senn.fcitx.ime:make-initial-state
                          (service-ime service)))))
    (format nil "~A" input-mode)))

(defun select-candidate (service index)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (service-state service)
    (if (eq input-mode :hiragana)
        (destructuring-bind (output state)
            (senn.fcitx.ime:select-candidate
             fcitx-state (service-ime service) index)
          (when state
            (setf fcitx-state state))
          output)
        (senn.fcitx.ime:make-output nil))))

(defun reset-im (service)
  (setf (service-state service) (make-initial-state))
  (values))
