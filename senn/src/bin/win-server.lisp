(defpackage :senn.bin.win-server
  (:use :cl)
  (:export :run
           :run-engine))
(in-package :senn.bin.win-server)

(defun run (kkc)
  (senn.server.tcp:start-server
   (lambda (client)
     (let ((ime (senn.win.stateful-ime:hachee-make-ime kkc)))
       (labels ((handle (req)
                  (senn.win.server:handle-request ime req)))
         (senn.server:client-loop client :handle-fn #'handle))))))

(defun run-engine (runner)
  (senn.server.tcp:start-server
   (lambda (client)
     (let ((ime (senn.win.stateful-ime:engine-make-ime runner)))
       (unwind-protect
            (labels ((handle (req)
                       (senn.win.server:handle-request ime req)))
              (senn.server:client-loop client :handle-fn #'handle))
         (senn.win.stateful-ime:engine-close-ime ime))))))
