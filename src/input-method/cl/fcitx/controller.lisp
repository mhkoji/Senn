(defpackage :hachee.input-method.fcitx.controller
  (:use :cl
        :hachee.input-method.op)
  (:import-from :alexandria
                :when-let)
  (:export :process-client
           :make-controller))
(in-package :hachee.input-method.fcitx.controller)

(defstruct controller id state)

(defun process-client (controller &key reader writer)
  (when-let ((line (funcall reader)))
    (let ((expr (as-expr line)))
      (log:info "[~A] expr: ~A" (controller-id controller) expr)
      (ecase (expr-op expr)
        (:do-input
         (destructuring-bind (new-state view)
             (hachee.input-method.stateful:transit
              (controller-state controller)
              (expr-arg expr "code"))
           (log:info "~A" view)
           (funcall writer (format nil "~A~%" view))
           (setf (controller-state controller) new-state))))
      (process-client controller :reader reader :writer writer))))
