(in-package :senn.lib.win)

(defun make-ime (engine-path)
  (let ((runner (senn.im.kkc.engine:make-engine-runner
                 :program engine-path)))
    (senn.win.stateful-ime:engine-make-ime runner)))

(defun close-ime (ime)
  (log:info "IME closing...")
  (senn.win.stateful-ime:engine-close-ime ime))
