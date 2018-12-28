(defpackage :hachee.input-method.fcitx.controller
  (:use :cl
        :hachee.input-method.op)
  (:import-from :alexandria
                :when-let)
  (:import-from :hachee.input-method.fcitx.state
                :transit-by-input
                :state-type
                :state-buffer
                :state-cursor-pos)
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
          (let ((new-state (transit-by-input (controller-state controller)
                                             (expr-arg expr "code"))))
            (log:info "~A" new-state)
            (funcall writer (format nil "~A ~A ~A~%"
                                    (state-type new-state)
                                    (state-buffer new-state)
                                    (state-cursor-pos new-state)))
            (setf (controller-state controller) new-state))))
      (process-client controller :reader reader :writer writer))))
