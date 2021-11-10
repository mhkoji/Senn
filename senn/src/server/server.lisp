(defpackage :senn.server
  (:use :cl)
  (:export :read-request
           :send-response
           :handle-request
           :client-loop))
(in-package :senn.server)

(defgeneric handle-request (handler req))
(defgeneric read-request (client))
(defgeneric send-response (client resp))

(defun client-loop (handler client)
  (handler-case
      (loop for req = (read-request client)
            while req
            do (let ((resp (handle-request handler req)))
                 (send-response client resp)))
    (error (c)
      (log:warn "~A" c))))
