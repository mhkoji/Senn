(in-package :senn.lib.fcitx)

(defun make-ime (engine-path)
  (let ((runner (senn.im.kkc.engine:make-engine-runner
                 :program engine-path)))
    (senn.fcitx.stateful-ime-engine:make-ime runner)))

(defun close-ime (ime)
  (log:info "IME closing...")
  (senn.fcitx.stateful-ime-engine:close-ime ime))
