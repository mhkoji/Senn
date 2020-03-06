(defpackage :senn.fcitx.stateful-im
  (:use :cl
        :hachee.ipc.op)
  (:export :loop-handling-request)
  (:import-from :alexandria
                :when-let))
(in-package :senn.fcitx.stateful-im)

(defun handle-request (expr state ime client)
  (ecase (expr-op expr)
    (:transit
     (let ((key (senn.fcitx.transit.keys:make-key
                 :sym (expr-arg expr "sym")
                 :state (expr-arg expr "state"))))
       (destructuring-bind (new-state view-update)
           (senn.fcitx.transit:transit ime state key)
         (senn.fcitx.net:send-response client view-update)
         new-state)))))

(defun loop-handling-request (ime client)
  (labels ((iter (state)
             (when-let ((expr (senn.fcitx.net:read-request client)))
               (let ((new-state (handle-request expr state ime client)))
                 (iter new-state)))))
    (iter (senn.fcitx.transit:make-initial-state ime))))
