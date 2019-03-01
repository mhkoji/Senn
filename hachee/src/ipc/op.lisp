(defpackage :hachee.ipc.op
  (:use :cl)
  (:import-from :cl-arrows :->)
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
      (-> expr (jsown:val "args") (jsown:val name))
    (error () nil)))
