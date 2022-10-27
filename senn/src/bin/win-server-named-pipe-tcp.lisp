(defpackage :senn.bin.win-server
  (:use :cl)
  (:export :run))
(in-package :senn.bin.win-server)

(defun run (host-str port-str)
  (let ((port (parse-integer port-str)))
    (senn.bin.win-server.named-pipe:start
     (lambda (cb)
       (senn.win.server:with-ime (ime host-str port)
         (funcall cb ime))))))
