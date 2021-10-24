(defpackage :senn.win.ipc
  (:use :cl
        :hachee.ipc.op)
  (:export :loop-handling-request
           :read-request
           :send-response)
  (:import-from :alexandria
                :when-let))
(in-package :senn.win.ipc)

(defgeneric read-request (c))

(defgeneric send-response (c resp))

(defclass service ()
  ((ime
    :initarg :ime
    :reader service-ime)
   (state
    :initarg :state
    :accessor service-state)))

(defun handle-request (service expr)
  (with-accessors ((ime service-ime)
                   (state service-state)) service
    (case (expr-op expr)
      (:process-input
       (let ((key (senn.win.keys:make-key
                   :code (expr-arg expr "keycode"))))
         (destructuring-bind (new-state ret-val)
             (senn.win.stateful-im:process-input ime state key)
           (setf state new-state)
           ret-val)))
      (:can-process
       (let ((key (senn.win.keys:make-key
                   :code (expr-arg expr "keycode"))))
         (senn.win.stateful-im:can-process ime state key))))))

(defun loop-handling-request (ime client)
  (let ((service (make-instance
                  'service
                  :ime ime
                  :state (senn.win.stateful-im:make-initial-state ime))))
    (loop for expr = (read-request client) while expr
          do (let ((resp (handle-request service expr)))
               (when resp
                 (send-response client resp))))))
