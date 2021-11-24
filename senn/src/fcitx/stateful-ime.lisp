(defpackage :senn.fcitx.stateful-ime
  (:use :cl)
  (:export :ime-state
           :set-ime-state
           :process-input
           :select-candidate
           :reset-im

           :stateful
           :make-initial-state
           :make-kkc-ime
           :make-engine-ime))
(in-package :senn.fcitx.stateful-ime)

(defgeneric ime-state (ime))
(defgeneric set-ime-state (ime state))

(defun make-initial-state ()
  (senn.fcitx.im:make-inputting))

(defun process-input (ime key)
  (with-accessors ((s ime-state)) ime
    (destructuring-bind (consumed-p view &key state)
        (senn.fcitx.im.process-input:execute ime s key)
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

;;;

(defclass stateful ()
  ((state :initarg :state)))

(defmethod ime-state ((ime stateful))
  (slot-value ime 'state))

(defmethod set-ime-state ((ime stateful) state)
  (setf (slot-value ime 'state) state))

;;;

(defclass stateful-kkc-ime (stateful
                            senn.im:ime
                            senn.im.mixin.kkc:convert
                            senn.im.mixin.kkc:lookup
                            senn.im.mixin.katakana:predict)
  ())

(defun make-kkc-ime (kkc)
  (make-instance 'stateful-kkc-ime
                 :state (make-initial-state)
                 :lookup-kkc-impl kkc
                 :convert-kkc-impl kkc))

;;;

#+sbcl
(defclass stateful-engine-ime (stateful
                               senn.im:ime
                               senn.im.mixin.engine:convert
                               senn.im.mixin.engine:lookup)
  ())

#+sbcl
(defun make-engine-ime (engine)
  (make-instance 'stateful-engine-ime
                 :convert-engine-impl engine
                 :lookup-engine-impl engine
                 :state (make-initial-state)))
