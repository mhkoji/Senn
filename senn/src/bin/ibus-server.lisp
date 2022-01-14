(defpackage :senn.bin.ibus-server
  (:use :cl)
  (:export :tcp-run))
(in-package :senn.bin.ibus-server)

(defun tcp-run (kkc)
  (senn.server.tcp:start-server
   (lambda (client)
     (let ((sf-ime (senn.ibus.stateful-ime:hachee-make-ime kkc)))
       (labels ((handle (req)
                  (senn.ibus.server:handle-request sf-ime req)))
         (senn.server:client-loop client :handle-fn #'handle))))))
