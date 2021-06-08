(defpackage :hachee.ipc.op
  (:use :cl)
  (:export :as-expr
           :expr-op
           :expr-arg))
(in-package :hachee.ipc.op)

(defun as-expr (string)
  (jsown:parse string))

(defun expr-op (expr)
  (alexandria:make-keyword (string-upcase (jsown:val expr "op"))))

(defun expr-arg (expr name)
  (handler-case
      (jsown:val (jsown:val expr "args")
                 name)
    (error () nil)))
