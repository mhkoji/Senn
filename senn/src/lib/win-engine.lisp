(in-package :senn.lib.win)

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
  (senn.im.kkc.engine:close-kkc (senn.win.stateful-ime:ime-kkc ime)))
