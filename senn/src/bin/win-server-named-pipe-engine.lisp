(defpackage :senn.bin.win-server
  (:use :cl)
  (:export :run))
(in-package :senn.bin.win-server)

(defun run ()
  (let ((kkc (senn.im.kkc.engine:start-kkc
              (senn.im.kkc.engine:make-engine-runner
               :program (merge-pathnames
                         ".senn/kkc-engine.exe"
                         (user-homedir-pathname))))))
    (unwind-protect
	 (senn.bin.win-server.named-pipe:start
	  (lambda (cb)
            (let ((ime (senn.win.stateful-ime:make-ime
                        :kkc kkc :predictor nil)))
              (funcall cb ime))))
      (senn.im.kkc.engine:close-kkc kkc))))
