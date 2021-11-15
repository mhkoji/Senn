(defpackage :senn.server
  (:use :cl)
  (:export :read-request
           :send-response
           :client-loop))
(in-package :senn.server)

(defgeneric read-request (client))
(defgeneric send-response (client resp))

(defun client-loop (client &key handle-fn)
  (handler-case
      (loop for req = (read-request client)
            while req
            do (let ((resp (funcall handle-fn req)))
                 (send-response client resp)))
    (error (c)
      (log:warn "~A" c))))
