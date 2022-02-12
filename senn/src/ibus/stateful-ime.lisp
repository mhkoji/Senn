(defpackage :senn.ibus.stateful-ime
  (:use :cl)
  (:export :process-input
           :toggle-input-mode
           :select-candidate
           :reset-im
           :make-ime))
(in-package :senn.ibus.stateful-ime)

(defgeneric ime-state (ime))

(defstruct state
  edit-state
  input-mode)

(defun make-initial-state ()
  (make-state
   :edit-state nil
   :input-mode :direct))

(defun reset-im (ime)
  (with-accessors ((input-mode state-input-mode)
                   (edit-state state-edit-state)) (ime-state ime)
    (setf input-mode :direct)
    (setf edit-state nil))
  "OK")

(defun process-input (ime key)
  (with-accessors ((input-mode state-input-mode)
                   (edit-state state-edit-state)) (ime-state ime)
    (if (eq input-mode :hiragana)
        (destructuring-bind (consumed-p view &key state)
            (senn.fcitx.im.process-input:execute edit-state ime key)
          (when state
            (setf edit-state state))
          (format nil "~A ~A"
                  (if consumed-p 1 0)
                  (if (and consumed-p view) view "NONE")))
        "0 NONE")))

(defun toggle-input-mode (ime)
  (with-accessors ((input-mode state-input-mode)
                   (edit-state state-edit-state)) (ime-state ime)
    (ecase input-mode
      (:hiragana
       (setf input-mode :direct)
       (setf edit-state nil))
      (:direct
       (setf input-mode :hiragana)
       (setf edit-state (senn.im.inputting:make-state))))
    (format nil "~A" input-mode)))

(defun select-candidate (ime index)
  (with-accessors ((input-mode state-input-mode)
                   (edit-state state-edit-state)) (ime-state ime)
    (if (eq input-mode :hiragana)
        (destructuring-bind (consumed-p view &key state)
            (senn.fcitx.im.select-candidate:execute edit-state index)
          (when state
            (setf edit-state state))
          (format nil "~A ~A"
                  (if consumed-p 1 0)
                  (if (and consumed-p view) view "NONE")))
        "0 NONE")))
;;;

(defclass ime (senn.fcitx.im:ime)
  ((state
    :initarg :state
    :reader ime-state)
   (kkc
    :initarg :kkc
    :reader senn.fcitx.im:ime-kkc)))

(defmethod senn.fcitx.im:ime-max-candidate-count ((ime ime))
  ;; An error occurs if the candidate count >= 16.
  15)

(defun make-ime (&key kkc)
  (make-instance 'ime
                 :state (make-initial-state)
                 :kkc kkc))
