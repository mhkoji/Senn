(defpackage :senn-ipc.server
  (:use :cl)
  (:export :client-read-line
           :client-send-line
           :client-loop))
(in-package :senn-ipc.server)

(defgeneric client-read-line (client))
(defgeneric client-send-line (client resp))

(defun client-loop (client &key handle-fn)
  (handler-case
      (loop for line = (client-read-line client) while line do
        (let ((resp (funcall handle-fn line)))
          (client-send-line client resp)))
    (error (c)
      (senn-ipc.server.log:warn "~A" c))))
