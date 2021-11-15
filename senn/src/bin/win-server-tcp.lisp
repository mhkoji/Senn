(defpackage :senn.bin.win-server-tcp
  (:use :cl)
  (:export :run))
(in-package :senn.bin.win-server-tcp)

(defun run (kkc)
  (senn.server.tcp:start-server
   (lambda (client)
     (let ((state (senn.win.stateful-ime:make-initial-state)))
       (let ((sf-ime (senn.win.stateful-ime:make-ime kkc state)))
         (labels ((handle (req)
                    (senn.win.server:handle-request sf-ime req)))
           (senn.server:client-loop client :handle-fn #'handle)))))))
