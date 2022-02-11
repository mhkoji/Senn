(defpackage :senn.fcitx.stateful-ime
  (:use :cl)
  (:export :process-input
           :select-candidate
           :reset-im
           :make-ime))
(in-package :senn.fcitx.stateful-ime)

(defstruct state
  edit-state)

(defun make-initial-state ()
  (make-state
   :edit-state
   (senn.im.inputting:make-state)))

(defgeneric ime-state (ime))

(defun process-input (ime key)
  (with-accessors ((es state-edit-state)) (ime-state ime)
    (destructuring-bind (consumed-p view &key state)
        (senn.fcitx.im.process-input:execute es ime key)
      (when state
        (setf es state))
      (format nil "~A ~A"
              (if consumed-p 1 0)
              (if (and consumed-p view)
                  (senn.fcitx.im.json:from-view view)
                  "NONE")))))

(defun select-candidate (ime index)
  (with-accessors ((es state-edit-state)) (ime-state ime)
    (destructuring-bind (consumed-p view &key state)
        (senn.fcitx.im.select-candidate:execute es index)
      (when state
        (setf es state))
      (format nil "~A ~A"
              (if consumed-p 1 0)
              (if (and consumed-p view)
                  (senn.fcitx.im.json:from-view view)
                  "NONE")))))

(defun reset-im (ime)
  (with-accessors ((es state-edit-state)) (ime-state ime)
    (setf es (senn.im.inputting:make-state)))
  "OK")

;;;

(defclass ime (senn.fcitx.im:ime)
  ((state
    :initarg :state
    :reader ime-state)
   (kkc
    :initarg :kkc
    :reader senn.fcitx.im:ime-kkc)
   (predictor
    :initarg :predictor
    :initform nil
    :reader senn.fcitx.im:ime-predictor)))

(defun make-ime (&key kkc predictor)
  (make-instance 'ime
                 :state (make-initial-state)
                 :kkc kkc
                 :predictor predictor))
