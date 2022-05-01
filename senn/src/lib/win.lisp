(defpackage :senn.lib.win
  (:use :cl)
  (:import-from :senn.win.server
                :handle-request)
  (:export :make-ime
           :close-ime
           :handle-request))

#+nil
(progn
  (defun make-ime (engine-path)
    (let ((engine-runner (senn.im.kkc.engine:make-engine-runner
			  :program engine-path)))
      (senn.win.stateful-ime:make-ime
       :kkc (make-instance 'senn.im.kkc.engine:kkc
	     :engine-store
	     (senn.im.kkc.engine:make-engine-store
	      :engine (senn.im.kkc.engine:run-engine engine-runner)
	      :engine-runner engine-runner))
       :predictor nil)))

  (defun close-ime (ime)
    (log:info "IME closing...")
    (senn.im.kkc.engine:close-kkc (senn.win.stateful-ime:ime-kkc ime))))

(progn
  (defun make-ime (pipe-name)
      (senn.win.stateful-ime:make-ime
       :kkc (senn.im.kkc.named-pipe:make-kkc-and-connect pipe-name)
       :predictor nil))

  (defun close-ime (ime)
    (log:info "IME closing...")
    (senn.im.kkc.named-pipe:close-kkc (senn.win.stateful-ime:ime-kkc ime))))
