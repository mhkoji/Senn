(defpackage :senn.fcitx.stateful-im
  (:use :cl
        :hachee.ipc.op)
  (:export :loop-handling-request
           :read-request
           :send-response)
  (:import-from :alexandria
                :when-let))
(in-package :senn.fcitx.stateful-im)

(defgeneric read-request (c))
(defgeneric send-response (c resp))

(defun handle-request (expr state ime client)
  (ecase (expr-op expr)
    (:transit
     (let ((key (senn.fcitx.transit.keys:make-key
                 :sym (expr-arg expr "sym")
                 :state (expr-arg expr "state"))))
       (destructuring-bind (new-state view-update)
           (senn.fcitx.transit:transit ime state key)
         (send-response client view-update)
         new-state)))))

(defun loop-handling-request (state ime client)
  (when-let ((expr (read-request client)))
    (let ((new-state (handle-request expr state ime client)))
      (loop-handling-request new-state ime client))))
