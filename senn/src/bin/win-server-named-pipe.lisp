(defpackage :senn.bin.win-server.named-pipe
  (:use :cl)
  (:export :start))
(in-package :senn.bin.win-server.named-pipe)

(defun start (call-with-ime-fn)
  (senn-ipc.server.named-pipe:start-server
   (lambda (client)
     (funcall call-with-ime-fn
      (lambda (ime)
        (labels ((handle (req)
                   (senn.win.server:handle-request ime req)))
          (senn-ipc.server:client-loop client :handle-fn #'handle)))))
   :pipe-name "\\\\.\\Pipe\\senn\\senn\\bin\\win-server\\run"))
