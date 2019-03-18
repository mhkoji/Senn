(defpackage :senn.win.server
  (:use :cl
        :hachee.ipc.op)
  (:export :loop-handling-request
           :read-request
           :send-response)
  (:import-from :alexandria
                :when-let))
(in-package :senn.win.server)

(defgeneric read-request (c))
(defgeneric send-response (c resp))

(defun make-response (s)
  (format nil "~A ~A~%"
          (symbol-name (type-of s))
          (senn.win.states:to-view s)))

(defun handle-request (expr state im client)
  (case (expr-op expr)
    (:input
     (let ((key (senn.win.keys:make-key
                 :code (expr-arg expr "keycode"))))
       (let ((new-state (senn.win.im:input im state key)))
         (let ((resp (make-response new-state)))
           (send-response client resp))
         new-state)))))

(defun loop-handling-request (state im client)
  (when-let ((expr (read-request client)))
    (when-let ((new-state (handle-request expr state im client)))
      (loop-handling-request new-state im client))))
