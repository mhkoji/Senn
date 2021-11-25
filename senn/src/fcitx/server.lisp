(defpackage :senn.fcitx.server
  (:use :cl)
  (:export :handle-request))
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
       (senn.fcitx.stateful-ime:select-candidate
        stateful-ime
        (hachee.ipc.op:expr-arg expr "index"))))))
