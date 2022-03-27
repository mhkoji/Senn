(defpackage :senn.fcitx.stateful-ime
  (:use :cl)
  (:export :process-input
           :select-candidate
           :reset-im
           :ime-kkc-store
           :make-ime))
(in-package :senn.fcitx.stateful-ime)

(defstruct state
  edit-state)

(defun make-initial-state ()
  (make-state
   :edit-state
   (senn.im.inputting:make-state)))

;;;

(defgeneric reload-kkc (ime))

(defgeneric ime-state (ime))

(defun process-input (ime key)
  (with-accessors ((es state-edit-state)) (ime-state ime)
    (destructuring-bind (consumed-p view &key state)
        (senn.fcitx.im.process-input:execute es ime key)
      (when state
        (setf es state))
      (format nil "~A ~A"
              (if consumed-p 1 0)
              (if (and consumed-p view) view "NONE")))))

(defun select-candidate (ime index)
  (with-accessors ((es state-edit-state)) (ime-state ime)
    (destructuring-bind (consumed-p view &key state)
        (senn.fcitx.im.select-candidate:execute es index)
      (when state
        (setf es state))
      (format nil "~A ~A"
              (if consumed-p 1 0)
              (if (and consumed-p view) view "NONE")))))

(defun reset-im (ime)
  (with-accessors ((es state-edit-state)) (ime-state ime)
    (setf es (senn.im.inputting:make-state)))
  "OK")

;;;

(defclass ime (senn.fcitx.im:ime)
  ((state
    :initarg :state)
   (kkc-store
    :initarg :kkc-store)
   (predictor
    :initarg :predictor
    :initform nil)))

(defmethod reload-kkc ((ime ime))
  (senn.im.kkc-store:reload (slot-value ime 'kkc-store))
  "OK")

(defmethod ime-state ((ime ime))
  (slot-value ime 'state))

(defmethod senn.fcitx.im:ime-kkc ((ime ime))
  (senn.im.kkc-store:get-kkc (slot-value ime 'kkc-store)))

(defmethod senn.fcitx.im:ime-predictor ((ime ime))
  (slot-value ime 'predictor))

(defun make-ime (&key kkc-store predictor)
  (make-instance 'ime
                 :state (make-initial-state)
                 :kkc-store kkc-store
                 :predictor predictor))
