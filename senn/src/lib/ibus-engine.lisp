(in-package :senn.lib.ibus)

(defun make-ime ()
  (let ((runner (senn.im.mixin.engine:make-engine-runner
                 :program "/usr/lib/senn/kkc-engine")))
    (senn.ibus.stateful-ime-engine:make-ime runner)))

(defun close-ime (ime)
  (log:info "IME closing...")
  (senn.ibus.stateful-ime-engine:close-ime ime)
  #+sbcl 0)
