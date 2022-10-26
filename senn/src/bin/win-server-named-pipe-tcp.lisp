(defpackage :senn.bin.win-server
  (:use :cl)
  (:export :run))
(in-package :senn.bin.win-server)

(defun run (host port)
  (senn.bin.win-server.named-pipe:start
   (lambda (cb)
     (senn.win.server:with-ime (ime host port)
       (funcall cb ime)))))
