(defpackage :hachee.input-method.emacs.stdio-server
  (:use :cl
        :hachee.input-method.op
        :hachee.input-method.stateless)
  (:export :enter-loop))
(in-package :hachee.input-method.emacs.stdio-server)

(defun call-with-read-input (callback)
  (loop for line = (read-line *standard-input* nil nil)
        while line do (progn
                        (funcall callback *standard-output* line)
                        (force-output *standard-output*))))

(defun enter-loop (kkc)
  (call-with-read-input (lambda (stream line)
    (let ((expr (as-expr line)))
      (if (eql (expr-op expr) :quit)
          (return-from enter-loop nil)
          (format stream "~A~%" (kkc-eval kkc expr)))))))
