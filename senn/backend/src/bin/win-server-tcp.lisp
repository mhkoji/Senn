(defpackage :senn.bin.win-server-unix
  (:use :cl)
  (:export :run))
(in-package :senn.bin.win-server-unix)

(defun run (kkc)
  (senn.server.tcp:start-server
   (lambda (client)
     (senn.win.server:loop-handling-request kkc
      :read-fn (lambda ()
                 (senn.server.tcp:read-request client))
      :send-fn (lambda (resp)
                 (senn.server.tcp:send-response client resp))))))
