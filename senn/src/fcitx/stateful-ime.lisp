(defpackage :senn.fcitx.stateful-ime
  (:use :cl)
  (:export :ime
           :make-initial-state
           :process-input
           :select-candidate
           :reset-im))
(in-package :senn.fcitx.stateful-ime)

(defclass ime (senn.fcitx.im:ime)
  ((state :initarg :state)))

(defun ime-state (ime)
  (slot-value ime 'state))

(defun set-ime-state (ime state)
  (setf (slot-value ime 'state) state))

(defun make-initial-state ()
  (senn.im.inputing:make-state))

(defun process-input (ime key)
  (with-accessors ((s ime-state)) ime
    (destructuring-bind (consumed-p view &key state)
        (senn.fcitx.im.process-input:execute s ime key)
      (when state
        (set-ime-state ime state))
      (format nil "~A ~A"
              (if consumed-p 1 0)
              (if (and consumed-p view) view "NONE")))))

(defun select-candidate (ime index)
  (with-accessors ((s ime-state)) ime
    (destructuring-bind (consumed-p view &key state)
        (senn.fcitx.im.select-candidate:execute s index)
      (when state
        (set-ime-state ime state))
      (format nil "~A ~A"
              (if consumed-p 1 0)
              (if (and consumed-p view) view "NONE")))))

(defun reset-im (ime)
  (set-ime-state ime (make-initial-state))
  "OK")
