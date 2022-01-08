(defpackage :senn.server
  (:use :cl)
  (:export :client-read-line
           :client-send-line
           :client-loop))
(in-package :senn.server)

(defgeneric client-read-line (client))
(defgeneric client-send-line (client resp))

(defun client-loop (client &key handle-fn)
  (handler-case
      (loop for req = (client-read-line client)
            while req
            do (let ((resp (funcall handle-fn req)))
                 (client-send-line client resp)))
    (error (c)
      (log:warn "~A" c))))
