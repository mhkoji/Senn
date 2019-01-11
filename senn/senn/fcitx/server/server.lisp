(defpackage :senn.fcitx.server
  (:use :cl
        :senn.op)
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

(defun loop-handling-request (state im client)
  (when-let ((expr (read-request client)))
    (ecase (expr-op expr)
      (:input
       (destructuring-bind (new-state input-return-value)
           (let ((key (senn.fcitx.im:make-key
                       :sym (expr-arg expr "sym")
                       :state (expr-arg expr "state"))))
             (senn.fcitx.im:input im state key))
         (let ((resp (make-response new-state input-return-value)))
           (send-response client resp))
         (loop-handling-request new-state im client))))))
