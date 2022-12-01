(in-package :senn.lib.ibus)

(defun make-ime (engine-path)
  (log:info "Making IME ...")
  (let ((kkc (senn.im.kkc.engine:make-kkc-and-run
              (senn.im.kkc.engine:make-engine-runner
               :program engine-path))))
    (senn.ibus.stateful-ime:make-ime :kkc kkc)))

(defun close-ime (ime)
  (log:info "Closing IME ...")
  (let ((kkc (senn.ibus.stateful-ime:ime-kkc ime)))
    (senn.im.kkc.engine:close-kkc kkc)))
