(defpackage :senn.win.server
  (:use :cl)
  (:export :loop-handling-request)
  (:import-from :alexandria
                :when-let))
(in-package :senn.win.server)

(defun handle-request (stateful-ime expr)
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
       :code (hachee.ipc.op:expr-arg expr "keycode"))))))

(defun loop-handling-request (kkc &key read-fn send-fn)
  (handler-case
      (let ((sf-ime (senn.win.stateful-ime:make-from-kkc kkc)))
        (loop for expr = (funcall read-fn) while expr
              do (let ((resp (handle-request sf-ime expr)))
                   (funcall send-fn resp))))
    (error (c)
      (log:warn "~A" c))))
