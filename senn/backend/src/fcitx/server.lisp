(defpackage :senn.fcitx.server
  (:use :cl)
  (:export :loop-handling-request))
(in-package :senn.fcitx.server)

(defun handle-request (stateful-ime line)
  (let ((expr (hachee.ipc.op:as-expr line)))
    (ecase (hachee.ipc.op:expr-op expr)
      (:process-input
       (senn.fcitx.stateful-ime:process-input
        stateful-ime
        (senn.fcitx.keys:make-key
         :sym (hachee.ipc.op:expr-arg expr "sym")
         :state (hachee.ipc.op:expr-arg expr "state"))))
      (:reset-im
       (senn.fcitx.stateful-ime:reset-im
        stateful-ime))
      (:select-candidate
       "1"))))

(defun loop-handling-request (kkc &key read-fn send-fn)
  (handler-case
      (let ((sf-ime (senn.fcitx.stateful-ime:make-from-kkc kkc)))
        (loop for req = (funcall read-fn) while req
              do (let ((resp (handle-request sf-ime req)))
                   (funcall send-fn resp))))
    (error (c)
      (log:warn "~A" c))))
