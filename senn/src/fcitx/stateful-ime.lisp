(defpackage :senn.fcitx.stateful-ime
  (:use :cl)
  (:export :stateful-ime
           :process-input
           :reset-im

           :make-initial-state
           :make-from-kkc))
(in-package :senn.fcitx.stateful-ime)

(defclass stateful-ime ()
  ((state
    :initarg :state
    :accessor stateful-ime-state)))

(defun make-initial-state ()
  (senn.fcitx.im:make-inputting))

(defun process-input (stateful-ime key)
  (with-accessors ((s stateful-ime-state)) stateful-ime
    (destructuring-bind (consumed-p view &key state)
        (senn.fcitx.im.process-input:execute stateful-ime s key)
      (when state
        (setf s state))
      (format nil "~A ~A"
              (if consumed-p 1 0)
              (if (and consumed-p view) view "NONE")))))

(defun reset-im (stateful-ime)
  (setf (stateful-ime-state stateful-ime) (make-initial-state))
  "OK")

;;;

(defclass ime (senn.im:ime
               senn.im.mixin:convert-kkc
               senn.im.mixin:lookup-kkc
               senn.im.mixin:predict-katakana
               stateful-ime)
  ())

(defun make-from-kkc (kkc)
  (make-instance 'ime
                 :lookup-kkc-impl kkc
                 :convert-kkc-impl kkc
                 :state (make-initial-state)))
