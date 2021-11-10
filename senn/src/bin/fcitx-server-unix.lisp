(defpackage :senn.bin.fcitx-server-unix
  (:use :cl)
  (:export :run))
(in-package :senn.bin.fcitx-server-unix)

(defstruct handler stateful-ime)

(defmethod senn.server:handle-request ((handler handler) req)
  (senn.fcitx.server:handle-request (handler-stateful-ime handler) req))

(defun run (kkc)
  (senn.server.unix:start-server
   (lambda ()
     (let ((sf-ime (senn.fcitx.stateful-ime:make-from-kkc kkc)))
       (make-handler :stateful-ime sf-ime)))))
