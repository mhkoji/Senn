(defpackage :senn.emacs.stdio-server
  (:use :cl :hachee.ipc.op)
  (:export :enter-loop)
  (:import-from :alexandria
                :when-let))
(in-package :senn.emacs.stdio-server)

(defun process-client (ime &key reader writer)
  (when-let ((line (funcall reader)))
    (let ((expr (as-expr line)))
      (when (not (eql (expr-op expr) :quit))
        (let ((result (senn.emacs.ime:process ime expr)))
          (funcall writer result))
        (process-client ime :reader reader :writer writer)))))


(defun enter-loop (ime)
  (process-client ime
   :reader (lambda ()
             (read-line *standard-input* nil nil))
   :writer (lambda (line)
             (format *standard-output* "~A~%" line)
             (force-output *standard-output*))))
