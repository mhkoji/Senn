(defpackage :senn.win.stateful-im
  (:use :cl
        :hachee.ipc.op)
  (:export :loop-handling-request
           :read-request
           :send-response)
  (:import-from :alexandria
                :when-let))
(in-package :senn.win.stateful-im)

(defgeneric read-request (c))

(defgeneric send-response (c resp))

(defclass stateful-im ()
  ((ime
    :initarg :ime
    :reader stateful-im-ime)
   (state
    :initarg :state
    :accessor stateful-im-state)))

(defun handle-request (stateful-im expr)
  (with-accessors ((ime stateful-im-ime)
                   (state stateful-im-state)) stateful-im
    (case (expr-op expr)
      (:process-input
       (let ((key (senn.win.keys:make-key
                   :code (expr-arg expr "keycode"))))
         (destructuring-bind (new-state ret-val)
             (senn.win.im:process-input ime state key)
           (setf state new-state)
           ret-val)))
      (:can-process
       (let ((key (senn.win.keys:make-key
                   :code (expr-arg expr "keycode"))))
         (senn.win.im:can-process ime state key))))))

(defun loop-handling-request (ime client)
  (let ((stateful-im (make-instance
                      'stateful-im
                      :ime ime
                      :state (senn.win.im:make-initial-state ime))))
    (loop for expr = (read-request client) while expr
          do (let ((resp (handle-request stateful-im expr)))
               (when resp
                 (send-response client resp))))))
