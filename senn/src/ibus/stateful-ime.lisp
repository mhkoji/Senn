(defpackage :senn.ibus.stateful-ime
  (:use :cl)
  (:export :ime-state
           :process-input
           :toggle-input-mode

           :stateful
           :make-initial-state
           :engine-make-ime
           :engine-close-ime
           :hachee-make-ime))
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
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state)) (ime-state ime)
    (if (eq input-mode :hiragana)
        (destructuring-bind (consumed-p view &key state)
            (senn.fcitx.im.process-input:execute ime input-state key)
          (when state
            (setf input-state state))
          (format nil "~A ~A"
                  (if consumed-p 1 0)
                  (if (and consumed-p view) view "NONE")))
        (format nil "~A ~A" 0 "NONE"))))

(defun toggle-input-mode (ime)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state)) (ime-state ime)
    (ecase input-mode
      (:hiragana
       (setf input-mode :direct)
       (setf input-state nil))
      (:direct
       (setf input-mode :hiragana)
       (setf input-state (senn.fcitx.im:make-inputting))))
    (format nil "~A" input-mode)))

;;;

(defclass stateful ()
  ((state :initarg :state)))

(defmethod ime-state ((ime stateful))
  (slot-value ime 'state))

;;;

(defclass stateful-hachee-ime (stateful
                               senn.im.ime:ime
                               senn.im.kkc.hachee:convert
                               senn.im.kkc.hachee:lookup
                               senn.im.predict.katakana:predict)
  ())

(defun hachee-make-ime (kkc)
  (let ((state (make-initial-state)))
    (make-instance 'stateful-hachee-ime :state state :kkc kkc)))

;;;

(defclass stateful-engine-ime (stateful
                               senn.im.ime:ime
                               senn.im.kkc.engine:convert
                               senn.im.kkc.engine:lookup)
  ())

(defun engine-make-ime (engine-runner)
  (make-instance 'stateful-engine-ime
   :state (make-initial-state)
   :engine-store (senn.im.kkc.engine:make-engine-store
                  :engine (senn.im.kkc.engine:run-engine
                           engine-runner)
                  :engine-runner engine-runner)))

(defun engine-close-ime (ime)
  (senn.im.kkc.engine:close-mixin ime))
