(in-package :senn.lib.fcitx)

(defun make-ime (engine-path)
  (format *error-output* "Making IME ...~%")
  (let ((kkc (senn.im.kkc.engine:start-kkc
              (senn.im.kkc.engine:make-engine-runner
               :program engine-path))))
    (senn.fcitx.stateful-ime:make-service :kkc kkc)))

(defun close-ime (ime)
  (format *error-output* "Closing IME ...~%")
  (let ((kkc (senn.fcitx.stateful-ime:service-kkc ime)))
    (senn.im.kkc.engine:close-kkc kkc)))
