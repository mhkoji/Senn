(defpackage :senn.ibus.stateful-ime
  (:use :cl)
  (:export :process-input
           :toggle-input-mode
           :select-candidate
           :reset-im
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

(defclass ime (senn.fcitx.im:ime)
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
        (destructuring-bind (resp state)
            (senn.fcitx.im:process-input ime fcitx-state key)
          (when state
            (setf fcitx-state state))
          resp)
        (list nil nil))))

(defun toggle-input-mode (ime)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (ime-state ime)
    (ecase input-mode
      (:hiragana
       (setf input-mode :direct)
       (setf fcitx-state nil))
      (:direct
       (setf input-mode :hiragana)
       (setf fcitx-state (senn.fcitx.im:make-initial-state))))
    (format nil "~A" input-mode)))

(defun select-candidate (ime index)
  (with-accessors ((input-mode state-input-mode)
                   (fcitx-state state-fcitx-state)) (ime-state ime)
    (if (eq input-mode :hiragana)
        (destructuring-bind (resp &key state)
            (senn.fcitx.im:select-candidate fcitx-state index)
          (when state
            (setf fcitx-state state))
          resp)
        (list nil nil))))

(defun reset-im (ime)
  (setf (ime-state ime) (make-initial-state))
  (values))

;;;

(defmethod senn.fcitx.im:ime-max-candidate-count ((ime ime))
  ;; An error occurs if the candidate count >= 16.
  15)

(defun make-ime (&key kkc)
  (make-instance 'ime
                 :state (make-initial-state)
                 :kkc kkc))
