(in-package :senn.lib.ibus)

(defun make-ime (engine-path)
  (format *error-output* "Making IME ...~%")
  (let ((kkc (senn.im.kkc.engine:start-kkc
              (senn.im.kkc.engine:make-engine-runner
               :program engine-path))))
    (senn.ibus.stateful-ime:make-ime :kkc kkc)))

(defun close-ime (ime)
  (format *error-output* "Closing IME ...~%")
  (let ((kkc (senn.ibus.stateful-ime:ime-kkc ime)))
    (senn.im.kkc.engine:close-kkc kkc)))
