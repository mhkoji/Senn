(in-package :senn.lib.fcitx)

(defun make-ime (engine-path)
  (let ((runner (senn.im.kkc.engine:make-engine-runner
                 :program engine-path)))
    (let ((store (senn.im.kkc-store.engine:make-store-and-run runner)))
      (senn.fcitx.stateful-ime:make-ime :kkc-store store))))

(defun close-ime (ime)
  (log:info "IME closing...")
  (let ((store (senn.fcitx.stateful-ime:ime-kkc-store ime)))
    (senn.im.kkc-store.engine:close-store store)))
