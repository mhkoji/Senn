(in-package :senn.lib.ibus)

(defun make-ime (engine-path)
  (format *error-output* "Making IME ...~%")
  (let ((kkc (senn.im.kkc.engine:start-kkc
              (senn.im.kkc.engine:make-engine-runner
               :program engine-path))))
    (make-instance 'senn.ibus.stateful-ime:service
     :ime (make-instance 'senn.ibus.im:ime :kkc kkc))))

(defun close-ime (ime)
  (format *error-output* "Closing IME ...~%")
  (let ((kkc (senn.ibus.im:ime-kkc
              (senn.ibus.stateful-ime:service-ime ime))))
    (senn.im.kkc.engine:close-kkc kkc)))
