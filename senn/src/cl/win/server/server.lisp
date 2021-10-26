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

(defun handle-request (stateful-ime expr)
  (case (expr-op expr)
    (:get-input-mode
     (senn.win.stateful-ime:get-input-mode
      stateful-ime))
    (:toggle-input-mode
     (senn.win.stateful-ime:toggle-input-mode
      stateful-ime))
    (:process-input
     (senn.win.stateful-ime:process-input
      stateful-ime
      (senn.win.keys:make-key :code (expr-arg expr "keycode"))))
    (:can-process
     (senn.win.stateful-ime:can-process
      stateful-ime
      (senn.win.keys:make-key :code (expr-arg expr "keycode"))))))

(defun loop-handling-request (ime client)
  (let ((stateful-ime (senn.win.stateful-ime:make-im ime)))
    (loop for expr = (read-request client) while expr
          do (let ((resp (handle-request stateful-ime expr)))
               (send-response client resp)))))
