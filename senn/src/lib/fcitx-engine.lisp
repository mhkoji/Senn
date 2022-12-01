(in-package :senn.lib.fcitx)

(defun make-ime (engine-path)
  (log:info "Making IME ...")
  (let ((kkc (senn.im.kkc.engine:make-kkc-and-run
              (senn.im.kkc.engine:make-engine-runner
               :program engine-path))))
    (senn.fcitx.stateful-ime:make-ime :kkc kkc)))

(defun close-ime (ime)
  (log:info "Closing IME ...")
  (let ((kkc (senn.fcitx.stateful-ime:ime-kkc ime)))
    (senn.im.kkc.engine:close-kkc kkc)))
