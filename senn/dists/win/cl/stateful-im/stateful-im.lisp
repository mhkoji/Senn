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
    (:transit
     (let ((key (senn.win.transit.keys:make-key
                 :code (expr-arg expr "keycode"))))
       (destructuring-bind (new-state view)
           (senn.win.transit:transit ime state key)
         (send-response client view)
         new-state)))))

(defun loop-handling-request (state ime client)
  (when-let ((expr (read-request client)))
    (when-let ((new-state (handle-request expr state ime client)))
      (loop-handling-request new-state ime client))))
