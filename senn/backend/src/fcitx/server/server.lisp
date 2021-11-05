(defpackage :senn.fcitx.server
  (:use :cl)
  (:export :read-request
           :send-response
           :loop-handling-request))
(in-package :senn.fcitx.server)

(defgeneric read-request (c))
(defgeneric send-response (c resp))

(defun handle-request (stateful-ime expr)
  (ecase (hachee.ipc.op:expr-op expr)
    (:process-input
     (senn.fcitx.stateful-ime:process-input
      stateful-ime
      (senn.fcitx.keys:make-key
       :sym (hachee.ipc.op:expr-arg expr "sym")
       :state (hachee.ipc.op:expr-arg expr "state"))))))

(defun loop-handling-request (stateful-ime client)
  (loop for expr = (read-request client) while expr
        do (let ((resp (handle-request stateful-ime expr)))
             (send-response client resp))))
