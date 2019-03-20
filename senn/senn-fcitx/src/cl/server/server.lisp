(defpackage :senn.fcitx.server
  (:use :cl
        :hachee.ipc.op)
  (:export :loop-handling-request
           :read-request
           :send-response)
  (:import-from :alexandria
                :when-let))
(in-package :senn.fcitx.server)

(defgeneric read-request (c))
(defgeneric send-response (c resp))

(defun make-response (s input-return-value)
  (format nil "~A ~A ~A~%"
          input-return-value
          (symbol-name (type-of s))
          (senn.fcitx.states:to-view s)))

(defun handle-request (expr state im client)
  (ecase (expr-op expr)
    (:transit
     (let ((key (senn.fcitx.keys:make-key
                 :sym (expr-arg expr "sym")
                 :state (expr-arg expr "state"))))
       (destructuring-bind (new-state input-return-value)
           (senn.fcitx.im:transit im state key)
         (let ((resp (make-response new-state input-return-value)))
           (send-response client resp))
         new-state)))))

(defun loop-handling-request (state im client)
  (when-let ((expr (read-request client)))
    (let ((new-state (handle-request expr state im client)))
      (loop-handling-request new-state im client))))
