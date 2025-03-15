(defpackage :senn.ibus.stateful-ime
  (:use :cl)
  (:export :process-input
           :toggle-input-mode
           :select-candidate
           :get-ime
           :get-kkc
           :reset-im
           :make-ime))
(in-package :senn.ibus.stateful-ime)

(defclass ime (senn.fcitx.im.immutable:ime-mixin)
  ((kkc
    :initarg :kkc
    :reader senn.fcitx.im.immutable:ime-kkc)))

(defmethod senn.fcitx.im.immutable:ime-max-candidate-count ((ime ime))
  ;; An error occurs if the candidate count >= 16.
  15)

;;;

(defstruct state
  input-mode
  fcitx-state)

(defun make-initial-state ()
  (make-state
   :input-mode :direct
   :fcitx-state nil))

;;;

(defclass stateful-ime ()
  ((ime
    :initarg :ime
    :reader get-ime)
   (state
    :initarg :state
    :accessor get-state)))

(defun make-ime (&key kkc)
  (make-instance 'stateful-ime
   :ime (make-instance 'ime :kkc kkc)
   :state (make-initial-state)))

(defun get-kkc (stateful-ime)
  (slot-value (get-ime stateful-ime) 'kkc))

;;;

(defun process-input (stateful-ime key)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (get-state stateful-ime)
    (if (eq input-mode :hiragana)
        (destructuring-bind (output state)
            (senn.fcitx.im.immutable:process-input
             fcitx-state key (get-ime stateful-ime))
          (when state
            (setf fcitx-state state))
          output)
        (senn.fcitx.im.immutable:make-output nil))))

(defun toggle-input-mode (stateful-ime)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (get-state stateful-ime)
    (ecase input-mode
      (:hiragana
       (setf input-mode :direct)
       (setf fcitx-state nil))
      (:direct
       (setf input-mode :hiragana)
       (setf fcitx-state (senn.fcitx.im.immutable:make-initial-state
                          (get-ime stateful-ime)))))
    (format nil "~A" input-mode)))

(defun select-candidate (stateful-ime index)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (get-state stateful-ime)
    (if (eq input-mode :hiragana)
        (destructuring-bind (output state)
            (senn.fcitx.im.immutable:select-candidate
             fcitx-state index (get-ime stateful-ime))
          (when state
            (setf fcitx-state state))
          output)
        (senn.fcitx.im.immutable:make-output nil))))

(defun reset-im (stateful-ime)
  (setf (get-state stateful-ime) (make-initial-state))
  (values))

;;;
