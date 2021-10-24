(defpackage :senn.win.server
  (:use :cl
        :hachee.ipc.op)
  (:export :loop-handling-request
           :read-request
           :send-response)
  (:import-from :alexandria
                :when-let))
(in-package :senn.win.server)

(defgeneric read-request (c))

(defgeneric send-response (c resp))

(defun handle-request (stateful-im expr)
  (case (expr-op expr)
    (:process-input
     (senn.win.stateful-im:process-input
      stateful-im
      (senn.win.keys:make-key :code (expr-arg expr "keycode"))))
    (:can-process
     (senn.win.stateful-im:can-process
      stateful-im
      (senn.win.keys:make-key :code (expr-arg expr "keycode"))))))

(defun loop-handling-request (ime client)
  (let ((stateful-im (senn.win.stateful-im:make-im ime)))
    (loop for expr = (read-request client) while expr
          do (let ((resp (handle-request stateful-im expr)))
               (send-response client resp)))))
