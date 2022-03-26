(defpackage :senn.bin.ibus-server
  (:use :cl)
  (:export :tcp-run
           :tcp-run-engine))
(in-package :senn.bin.ibus-server)

(defun ime-client-loop (client ime)
  (labels ((handle (req)
             (senn.ibus.server:handle-request ime req)))
    (senn.server:client-loop client :handle-fn #'handle)))

(defun tcp-run (kkc)
  (senn.server.tcp:start-server
   (lambda (client)
     (let ((ime (senn.ibus.stateful-ime-hachee:make-ime kkc)))
       (ime-client-loop client ime)))))

(defun tcp-run-engine (runner)
  (senn.server.tcp:start-server
   (lambda (client)
     (let ((ime (senn.ibus.stateful-ime-engine:make-ime runner)))
       (unwind-protect (ime-client-loop client ime)
         (senn.ibus.stateful-ime-engine:close-ime ime))))))
