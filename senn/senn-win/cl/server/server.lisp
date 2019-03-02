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

(defun loop-handling-request (state im client)
  (when-let ((expr (read-request client)))
    (print expr)
    (send-response client expr)
    (loop-handling-request state im client)))
