(defpackage :senn.bin.win-server
  (:use :cl)
  (:export :run))
(in-package :senn.bin.win-server)

(defun make-engine-runner ()
  (senn.im.kkc.engine:make-engine-runner
   :program (merge-pathnames ".senn/kkc-engine.exe"
                             (user-homedir-pathname))))

(defun run-with-kkc (kkc)
  (senn-ipc.server.named-pipe:start-server
   (lambda (client)
     (let ((ime (senn.win.stateful-ime:make-ime
                 :kkc kkc
                 :predictor nil)))
       (labels ((handle (req)
                  (senn.win.server:handle-request ime req)))
         (senn-ipc.server:client-loop client :handle-fn #'handle))))
   :pipe-name "\\\\.\\Pipe\\senn\\senn\\bin\\win-server\\run"))

(defun run ()
  (let ((kkc (senn.im.kkc.engine:make-kkc-and-run
              (make-engine-runner))))
    (unwind-protect (run-with-kkc kkc)
      (senn.im.kkc.engine:close-kkc kkc))))
