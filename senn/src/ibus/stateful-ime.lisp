(defpackage :senn.ibus.stateful-ime
  (:use :cl)
  (:export :ime-state
           :process-input
           :toggle-input-mode

           :stateful
           :make-initial-state
           :make-kkc-ime))
(in-package :senn.ibus.stateful-ime)

(defgeneric ime-state (ime))

(defstruct state
  input-state
  input-mode)

(defun make-initial-state ()
  (make-state
   :input-state (senn.fcitx.im:make-inputting)
   :input-mode :direct))

(defun process-input (ime key)
  (with-accessors ((input-state state-input-state)) (ime-state ime)
    (destructuring-bind (consumed-p view &key state)
        (senn.fcitx.im.process-input:execute ime input-state key)
      (when state
        (setf input-state state))
      (format nil "~A ~A"
              (if consumed-p 1 0)
              (if (and consumed-p view) view "NONE")))))

(defun toggle-input-mode (ime)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state)) (ime-state ime)
    (ecase input-mode
      (:hiragana
       (setf input-mode :direct)
       (setf input-state nil))
      (:direct
       (setf input-mode :hiragana)
       (setf input-state (senn.fcitx.im:make-inputting)))))
  (format nil "OK~%"))

;;;

(defclass stateful ()
  ((state :initarg :state)))

(defmethod ime-state ((ime stateful))
  (slot-value ime 'state))

;;;

(defclass stateful-kkc-ime (stateful
                            senn.im:ime
                            senn.im.mixin:convert-kkc
                            senn.im.mixin:lookup-kkc
                            senn.im.mixin:predict-katakana)
  ())

(defun make-kkc-ime (kkc)
  (make-instance 'stateful-kkc-ime
                 :state (make-initial-state)
                 :lookup-kkc-impl kkc
                 :convert-kkc-impl kkc))