(in-package :senn.lib.ibus)

(defun make-ime ()
  (senn.ibus.stateful-ime:make-engine-ime
   (senn.im.mixin.engine:make-engine-runner
    :program "/usr/lib/senn/kkc-engine")))

(defun close-ime (ime)
  (log:info "IME closing...")
  (senn.ibus.stateful-ime:close-engine-ime ime)
  #+sbcl 0)
