(defpackage :senn.bin.fcitx-server-unix
  (:use :cl)
  (:export :run))
(in-package :senn.bin.fcitx-server-unix)

(defun run (kkc)
  (senn.server.unix:start-server
   (lambda (client)
     (senn.fcitx.server:loop-handling-request kkc
      :read-fn (lambda ()
                 (senn.server.unix:read-request client))
      :send-fn (lambda (resp)
                 (senn.server.unix:send-response client resp))))))
