(in-package :senn.lib.fcitx)

(defun make-ime (engine-path)
  (log:info "Making IME ...")
  (let ((store (senn-kkc.store.engine:make-store-and-run
                (senn-kkc.engine:make-engine-runner
                 :program engine-path))))
    (senn.fcitx.stateful-ime:make-ime :kkc-store store)))

(defun close-ime (ime)
  (log:info "Closing IME ...")
  (let ((store (senn.fcitx.stateful-ime:ime-kkc-store ime)))
    (senn-kkc.store.engine:close-store store)))
