(defpackage :senn.bin.win-server-tcp
  (:use :cl)
  (:export :run))
(in-package :senn.bin.win-server-tcp)

(defstruct handler stateful-ime)

(defmethod senn.server:handle-request ((handler handler) req)
  (senn.win.server:handle-request (handler-stateful-ime handler) req))

(defun run (kkc)
  (senn.server.tcp:start-server
   (lambda ()
     (let ((sf-ime (senn.win.stateful-ime:make-from-kkc kkc)))
       (make-handler :stateful-ime sf-ime)))))
