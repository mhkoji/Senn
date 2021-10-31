(defpackage :senn.win.stateful-ime
  (:use :cl)
  (:export :make-ime
           :get-input-mode
           :toggle-input-mode
           :can-process
           :process-input))
(in-package :senn.win.stateful-ime)

(defstruct state
  input-mode
  input-state)

;;;

(defclass stateful-ime (senn.im:ime)
  ((ime  ;; stateless ime, instance of senn.im:ime
    :initarg :ime
    :reader stateful-ime-ime)
   (state
    :initarg :state
    :accessor stateful-ime-state)))

(defmethod senn.im:convert ((sf-ime stateful-ime) pron
                            &key 1st-boundary-index)
  (with-accessors ((ime stateful-ime-ime)
                   (state stateful-ime-state)) sf-ime
    (let ((segments (senn.im:convert
                     ime pron :1st-boundary-index 1st-boundary-index)))
      segments)))

(defmethod senn.im:lookup ((sf-ime stateful-ime) pron
                           &key prev next)
  (with-accessors ((ime stateful-ime-ime)) sf-ime
    (senn.im:lookup ime pron :next next :prev prev)))

(defun make-ime (ime)
  (make-instance 'stateful-ime
                 :ime ime
                 :state (make-state
                         :input-mode :direct
                         :input-state nil)))

(defun get-input-mode (stateful-ime)
  (format nil "~A~%" (state-input-mode (stateful-ime-state stateful-ime))))

(defun toggle-input-mode (stateful-ime)
  (with-accessors ((state stateful-ime-state)) stateful-ime
    (with-accessors ((input-mode state-input-mode)
                     (input-state state-input-state)) state
      (ecase input-mode
        (:hiragana
         (setf input-mode :direct)
         (setf input-state nil))
        (:direct
         (setf input-mode :hiragana)
         (setf input-state (senn.win.im:make-editing))))))
  ;; It seems to need to consume output buffer..
  (format nil "OK~%"))

(defun can-process (stateful-ime key)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state))
      (stateful-ime-state stateful-ime)
    (let ((can-process (senn.win.im.can-process:execute
                        stateful-ime input-state input-mode key)))
      (format nil "~A~%" (if can-process 1 0)))))

(defun process-input (stateful-ime key)
  (with-accessors ((input-mode state-input-mode)
                   (input-state state-input-state)
                   (history state-history))
      (stateful-ime-state stateful-ime)
    (let ((result (senn.win.im.process-input:execute
                   stateful-ime input-state input-mode key)))
      (destructuring-bind (can-process view
                           &key state)
          result
        (when state
          (setf input-state state))
        (format nil "~A ~A~%"
                (if (and can-process view) 1 0)
                (or view ""))))))
