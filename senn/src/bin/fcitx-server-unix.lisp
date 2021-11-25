(defpackage :senn.bin.fcitx-server-unix
  (:use :cl)
  (:export :run
           :run-engine))
(in-package :senn.bin.fcitx-server-unix)

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

(defun run (kkc)
  (senn.server.unix:start-server
   (lambda (client)
     (let ((sf-ime (senn.fcitx.stateful-ime:make-kkc-ime kkc)))
       (senn.server:client-loop client
        :handle-fn (lambda (req) (handle-request sf-ime req)))))))

#+sbcl
(defun run-engine (runner)
  (senn.server.unix:start-server
   (lambda (client)
     (senn.im.mixin.engine:with-engine (engine runner)
       (let ((sf-ime (senn.fcitx.stateful-ime:make-engine-ime engine)))
         (senn.server:client-loop client
          :handle-fn (lambda (req) (handle-request sf-ime req))))))))
