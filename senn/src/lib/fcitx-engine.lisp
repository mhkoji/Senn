(in-package :senn.lib.fcitx)

(defun make-ime ()
  (senn.fcitx.stateful-ime:make-engine-ime
   (senn.im.mixin.engine:run-engine
    (senn.im.mixin.engine:make-engine-runner
     :program "/usr/lib/senn/kkc-engine"))))

(defun close-ime (ime)
  (log:info "IME closing...")
  (senn.fcitx.stateful-ime:close-engine-ime ime))
