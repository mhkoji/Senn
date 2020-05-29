(defpackage :senn.win.stateful-im
  (:use :cl
        :hachee.ipc.op)
  (:export :loop-handling-request
           :read-request
           :send-response)
  (:import-from :alexandria
                :when-let))
(in-package :senn.win.stateful-im)

(defgeneric read-request (c))
(defgeneric send-response (c resp))

(defun handle-request (expr state ime client)
  (case (expr-op expr)
    (:process-input
     (let ((key (senn.win.input-processor.keys:make-key
                 :code (expr-arg expr "keycode"))))
       (destructuring-bind (new-state view)
           (senn.win.input-processor:process-input ime state key)
         (send-response client view)
         new-state)))))

(defun loop-handling-request (state ime client)
  (when-let ((expr (read-request client)))
    (when-let ((new-state (handle-request expr state ime client)))
      (loop-handling-request new-state ime client))))
