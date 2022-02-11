(defpackage :senn.ibus.stateful-ime
  (:use :cl)
  (:export :process-input
           :toggle-input-mode

           :engine-make-ime
           :engine-close-ime
           :hachee-make-ime))
(in-package :senn.ibus.stateful-ime)

(defgeneric ime-state (ime))

(defstruct state
  edit-state
  input-mode)

(defun make-initial-state ()
  (make-state
   :edit-state nil
   :input-mode :direct))

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
                  (if (and consumed-p view)
                      (senn.fcitx.im.json:from-view view)
                      "NONE")))
        (format nil "~A ~A" 0 "NONE"))))

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

;;;

(defclass ime (senn.fcitx.im:ime)
  ((state
    :initarg :state
    :reader ime-state)
   (kkc
    :initarg :kkc
    :reader senn.fcitx.im:ime-kkc)))

;;;

(defun hachee-make-ime (kkc)
  (make-instance 'ime
   :kkc (make-instance 'senn.im.kkc.hachee:kkc
         :impl kkc
         :state (senn.im.kkc.hachee:make-state))
   :state (make-initial-state)))

;;;

(defun engine-make-ime (engine-runner)
  (make-instance 'ime
   :kkc (make-instance 'senn.im.kkc.engine:kkc
         :engine-store
         (senn.im.kkc.engine:make-engine-store
          :engine (senn.im.kkc.engine:run-engine engine-runner)
          :engine-runner engine-runner))
   :state (make-initial-state)))

(defun engine-close-ime (ime)
  (senn.im.kkc.engine:close-kkc (slot-value ime 'kkc)))
