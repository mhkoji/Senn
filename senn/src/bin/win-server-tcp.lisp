(defpackage :senn.bin.win-server-tcp
  (:use :cl)
  (:export :run
           :run-engine))
(in-package :senn.bin.win-server-tcp)

(defun handle-request (stateful-ime line)
  (let ((expr (hachee.ipc.op:as-expr line)))
    (case (hachee.ipc.op:expr-op expr)
      (:get-input-mode
       (senn.win.stateful-ime:get-input-mode
        stateful-ime))
      (:toggle-input-mode
       (senn.win.stateful-ime:toggle-input-mode
        stateful-ime))
      (:process-input
       (senn.win.stateful-ime:process-input
        stateful-ime
        (senn.win.keys:make-key
         :code (hachee.ipc.op:expr-arg expr "keycode")
         :shift-p (hachee.ipc.op:expr-arg expr "shift"))))
      (:can-process
       (senn.win.stateful-ime:can-process
        stateful-ime
        (senn.win.keys:make-key
         :code (hachee.ipc.op:expr-arg expr "keycode")))))))

(defun run (kkc)
  (senn.server.tcp:start-server
   (lambda (client)
     (let ((state (senn.win.stateful-ime:make-initial-state)))
       (let ((sf-ime (senn.win.stateful-ime:make-ime kkc state)))
         (senn.server:client-loop client
          :handle-fn (lambda (req) (handle-request sf-ime req))))))))

(defun run-engine (runner)
  (senn.server.tcp:start-server
   (lambda (client)
     (senn.im.mixin.engine:with-engine (engine runner)
       (let ((state (senn.win.stateful-ime:make-initial-state)))
         (let ((sf-ime (senn.win.stateful-ime:make-engine-ime
                        engine
                        state)))
           (labels ((handle (req)
                      (senn.win.server:handle-request sf-ime req)))
             (senn.server:client-loop client :handle-fn #'handle))))))))
