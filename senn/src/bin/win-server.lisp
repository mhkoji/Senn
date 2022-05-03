(defpackage :senn.bin.win-server
  (:use :cl)
  (:export :run))
(in-package :senn.bin.win-server)

(defun run ()
  (senn-ipc.server.named-pipe:start-server
   (lambda (client)
     ;; TODO: verify that the kkc server is valid.
     (let ((kkc (senn.im.kkc.named-pipe:make-kkc-and-connect
		 "\\\\.\\Pipe\\senn\\senn-kkc-engine")))
       (unwind-protect
	    (let ((ime (senn.win.stateful-ime:make-ime
			:kkc kkc
			:predictor nil)))
	      (labels ((handle (req)
			 (senn.win.server:handle-request ime req)))
		(senn-ipc.server:client-loop client :handle-fn #'handle)))
	 (senn.im.kkc.named-pipe:close-kkc kkc))))
   :pipe-name "\\\\.\\Pipe\\senn\\senn\\bin\\win-server\\run"))
