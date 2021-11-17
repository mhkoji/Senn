(defpackage :senn.ibus.server
  (:use :cl)
  (:export :handle-request))
(in-package :senn.ibus.server)

(defun handle-request (stateful-ime line)
  (let ((expr (hachee.ipc.op:as-expr line)))
    (ecase (hachee.ipc.op:expr-op expr)
      (:process-input
       (senn.ibus.stateful-ime:process-input
        stateful-ime
        (senn.fcitx.keys:make-key
         :sym (hachee.ipc.op:expr-arg expr "sym")
         :state (hachee.ipc.op:expr-arg expr "state"))))
      (:toggle-input-mode
       (senn.ibus.stateful-ime:toggle-input-mode
        stateful-ime)))))
