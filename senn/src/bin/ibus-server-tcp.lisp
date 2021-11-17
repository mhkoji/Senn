(defpackage :senn.bin.ibus-server-tcp
  (:use :cl)
  (:export :run))
(in-package :senn.bin.ibus-server-tcp)

(defun run (kkc)
  (senn.server.tcp:start-server
   (lambda (client)
     (let ((sf-ime (senn.ibus.stateful-ime:make-kkc-ime kkc)))
       (labels ((handle (req)
                  (senn.ibus.server:handle-request sf-ime req)))
         (senn.server:client-loop client :handle-fn #'handle))))))
