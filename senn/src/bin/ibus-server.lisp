(defpackage :senn.bin.ibus-server
  (:use :cl)
  (:export :tcp-run
           :tcp-run-engine))
(in-package :senn.bin.ibus-server)

(defun make-hachee-ime (kkc)
  (senn.ibus.stateful-ime:make-ime
   :kkc (make-instance 'senn.im.kkc.hachee:kkc
                       :hachee-impl-lm-kkc kkc)))

(defmacro with-engine-ime ((ime runner) &body body)
  `(let ((kkc (senn.im.kkc.engine:start-kkc ,runner)))
     (unwind-protect
          (let ((,ime (senn.ibus.stateful-ime:make-ime :kkc kkc)))
            ,@body)
       (senn.im.kkc.engine:close-kkc kkc))))

(defun ime-client-loop (client ime)
  (labels ((handle (req)
             (senn.ibus.server:handle-request ime req)))
    (senn-ipc.server:client-loop client :handle-fn #'handle)))

(defun tcp-run (kkc)
  (senn-ipc.server.tcp:start-server
   (lambda (client)
     (let ((ime (make-hachee-ime kkc)))
       (ime-client-loop client ime)))))

(defun tcp-run-engine (runner)
  (senn-ipc.server.tcp:start-server
   (lambda (client)
     (with-engine-ime (ime runner)
       (ime-client-loop client ime)))))
