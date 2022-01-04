(in-package :senn.lib.fcitx)

(defun make-ime (engine-path)
  (senn.fcitx.stateful-ime:make-engine-ime
   (senn.im.mixin.engine:run-engine
    (senn.im.mixin.engine:make-engine-runner :program engine-path))))

(defun close-ime (ime)
  (log:info "IME closing...")
  (senn.fcitx.stateful-ime:close-engine-ime ime))
