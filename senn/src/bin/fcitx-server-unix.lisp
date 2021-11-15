(defpackage :senn.bin.fcitx-server-unix
  (:use :cl)
  (:export :run))
(in-package :senn.bin.fcitx-server-unix)

(defun run (kkc)
  (senn.server.unix:start-server
   (lambda (client)
     (let ((sf-ime (senn.fcitx.stateful-ime:make-kkc-ime kkc)))
       (labels ((handle (req)
                  (senn.fcitx.server:handle-request sf-ime req)))
         (senn.server:client-loop client :handle-fn #'handle))))))
