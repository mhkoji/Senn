(in-package :senn.lib.ibus)

(defun make-ime (engine-path)
  (let ((runner (senn.im.kkc.engine:make-engine-runner
                 :program engine-path)))
    (let ((kkc (make-instance 'senn.im.kkc.engine:kkc
                :engine-store
                (senn.im.kkc.engine:make-engine-store
                 :engine (senn.im.kkc.engine:run-engine engine-runner)
                 :engine-runner engine-runner))))
      (senn.ibus.stateful-ime:make-ime :kkc kkc))))

(defun close-ime (ime)
  (log:info "IME closing...")
  (let ((kkc (senn.fcitx.im:ime-kkc ime)))
    (senn.im.kkc.engine:close-kkc kkc)))
