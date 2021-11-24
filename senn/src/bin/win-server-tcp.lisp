(defpackage :senn.bin.win-server-tcp
  (:use :cl)
  (:export :run
           :run-engine))
(in-package :senn.bin.win-server-tcp)

(defun run (kkc)
  (senn.server.tcp:start-server
   (lambda (client)
     (let ((state (senn.win.stateful-ime:make-initial-state)))
       (let ((sf-ime (senn.win.stateful-ime:make-ime kkc state)))
         (labels ((handle (req)
                    (senn.win.server:handle-request sf-ime req)))
           (senn.server:client-loop client :handle-fn #'handle)))))))

(defun run-engine (runner)
  (senn.server.tcp:start-server
   (lambda (client)
     (senn.im.mixin.engine:with-engine (engine runner)
       (let ((state (senn.win.stateful-ime:make-initial-state)))
         (let ((sf-ime (senn.win.stateful-ime:make-engine-ime
                        engine
                        state)))
           (labels ((handle (req)
                      (senn.win.server:handle-request sf-ime req)))
             (senn.server:client-loop client :handle-fn #'handle))))))))
