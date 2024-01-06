(defpackage :senn.ibus.stateful-ime
  (:use :cl)
  (:export :process-input
           :toggle-input-mode
           :select-candidate
           :reset-im
           :ime-kkc
           :make-ime))
(in-package :senn.ibus.stateful-ime)

(defstruct state
  input-mode
  fcitx-state)

(defun make-initial-state ()
  (make-state
   :input-mode :direct
   :fcitx-state nil))

;;;

(defclass ime (senn.fcitx.im.immutable:ime)
  ((state
    :initarg :state
    :accessor ime-state)
   (kkc
    :initarg :kkc
    :reader ime-kkc)))

(defun process-input (ime key)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (ime-state ime)
    (if (eq input-mode :hiragana)
        (destructuring-bind (output state)
            (senn.fcitx.im.immutable.process-input:execute
             fcitx-state key ime)
          (when state
            (setf fcitx-state state))
          output)
        (senn.fcitx.im.immutable:view->output nil))))

(defun toggle-input-mode (ime)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (ime-state ime)
    (ecase input-mode
      (:hiragana
       (setf input-mode :direct)
       (setf fcitx-state nil))
      (:direct
       (setf input-mode :hiragana)
       (setf fcitx-state (senn.fcitx.im.immutable:make-initial-state))))
    (format nil "~A" input-mode)))

(defun select-candidate (ime index)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (ime-state ime)
    (if (eq input-mode :hiragana)
        (destructuring-bind (output state)
            (senn.fcitx.im.immutable.select-candidate:execute
             fcitx-state index)
          (when state
            (setf fcitx-state state))
          output)
        (senn.fcitx.im.immutable:view->output nil))))

(defun reset-im (ime)
  (setf (ime-state ime) (make-initial-state))
  (values))

;;;

(defmethod senn.fcitx.im.immutable:ime-kkc ((ime ime))
  (ime-kkc ime))

(defmethod senn.fcitx.im.immutable:ime-max-candidate-count ((ime ime))
  ;; An error occurs if the candidate count >= 16.
  15)

(defun make-ime (&key kkc)
  (make-instance 'ime
                 :state (make-initial-state)
                 :kkc kkc))
