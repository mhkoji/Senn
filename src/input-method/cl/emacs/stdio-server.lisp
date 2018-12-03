(defpackage :hachee.input-method.emacs.stdio-server
  (:use :cl
        :hachee.input-method.op
        :hachee.input-method.stateless)
  (:export :enter-loop)
  (:import-from :alexandria
                :when-let))
(in-package :hachee.input-method.emacs.stdio-server)

(defun process-client (kkc &key reader writer)
  (when-let ((line (funcall reader)))
    (let ((expr (as-expr line)))
      (when (not (eql (expr-op expr) :quit))
        (let ((result (kkc-eval kkc expr)))
          (funcall writer result))
        (process-client kkc :reader reader :writer writer)))))


(defun enter-loop (kkc)
  (process-client kkc
   :reader (lambda ()
             (read-line *standard-input* nil nil))
   :writer (lambda (line)
             (format *standard-output* "~A~%" line)
             (force-output *standard-output*))))
