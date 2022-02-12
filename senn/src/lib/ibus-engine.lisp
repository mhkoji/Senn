(in-package :senn.lib.ibus)

(defun make-ime (engine-path)
  (let ((runner (senn.im.kkc.engine:make-engine-runner
                 :program engine-path)))
    (senn.ibus.stateful-ime-engine:make-ime runner)))

(defun close-ime (ime)
  (log:info "IME closing...")
  (senn.ibus.stateful-ime-engine:close-ime ime))
