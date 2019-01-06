(defpackage :senn.fcitx.server
  (:use :cl
        :senn.op
        :senn.fcitx.states)
  (:export :process-client)
  (:import-from :senn.fcitx.server.client
                :transit-by-input)
  (:import-from :alexandria
                :when-let))
(in-package :senn.fcitx.server)

(defgeneric make-response (s consumed))

(defun process-client (client &key reader writer)
  (labels ((process-loop (state)
             (when-let ((line (funcall reader)))
               (let ((expr (as-expr line)))
                 (ecase (expr-op expr)
                   (:transit-by-input
                    (destructuring-bind (new-state &optional (consumed t))
                        (alexandria:ensure-list
                         (transit-by-input client state
                                           (expr-arg expr "code")))
                      (funcall writer (make-response new-state consumed))
                      (process-loop new-state))))))))
    (process-loop (make-editing))))
