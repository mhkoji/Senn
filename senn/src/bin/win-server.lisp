(defpackage :senn.bin.win-server
  (:use :cl)
  (:export :run))
(in-package :senn.bin.win-server)

(defun make-engine-runner ()
  (senn.im.kkc.engine:make-engine-runner
   :program (merge-pathnames ".senn/kkc/engine"
			     (user-homedir-pathname))))

(defun run-internal (kkc)
  (senn-ipc.server.named-pipe:run
   (lambda (client)
     (unwind-protect
	  (let ((ime (senn.win.stateful-ime:make-ime
		      :kkc kkc
		      :predictor nil)))
	    (labels ((handle (req)
		       (senn.win.server:handle-request ime req)))
	      (senn-ipc.server:client-loop client :handle-fn #'handle)))
       (senn.im.kkc.named-pipe:close-kkc kkc)))
   :pipe-name "\\\\.\\Pipe\\senn\\senn\\bin\\win-server\\run"))

(defun run ()
  (let ((kkc (senn.im.kkc.engine:make-kkc-and-run
	      (make-engine-runner))))
    (unwind-protect
	 (run-internal kkc)
      (senn.im.kkc.engine:close-kkc kkc))))
