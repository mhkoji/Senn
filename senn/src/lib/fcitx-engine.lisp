(in-package :senn.lib.fcitx)

(defun make-ime (engine-path)
  (format *error-output* "Making IME ...~%")
  (let ((kkc (senn.im.kkc.engine:start-kkc
              (senn.im.kkc.engine:make-engine-runner
               :program engine-path))))
    (make-instance 'senn.fcitx.stateful-ime:service
     :ime (make-instance 'senn.fcitx.im:ime :kkc kkc))))
                         

(defun close-ime (ime)
  (format *error-output* "Closing IME ...~%")
  (let ((kkc (senn.fcitx.im:ime-kkc
              (senn.fcitx.stateful-ime:service-ime ime))))
    (senn.im.kkc.engine:close-kkc kkc)))
