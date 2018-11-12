(defpackage :hachee.kkc-server
  (:use :cl)
  (:import-from :cl-arrows :->)
  (:export :main))
(in-package :hachee.kkc-server)

(defun call-with-read-input (callback)
  (loop for line = (read-line *standard-input* nil nil)
        while line do (funcall callback *standard-output* line)))


(defun as-expr (string)
  (jsown:parse string))

(defun expr-op (expr)
  (jsown:val expr "op"))

(defun expr-arg (expr name)
  (-> expr (jsown:val "args") (jsown:val name)))

(defun kkc-eval (kkc expr)
  (let ((op (expr-op expr)))
    (ecase (alexandria:make-keyword (string-upcase op))
      (:convert
       ;; {"op": "convert", "args": {"text": "あおぞらぶんこ"}}
       (hachee.kkc:convert kkc (expr-arg expr "text"))))))


(defun main (pathnames)
  (let* ((dictionary (hachee.kkc:build-dictionary pathnames))
         (vocabulary (hachee.kkc:build-vocabulary pathnames))
         (language-model (hachee.kkc:build-language-model
                          pathnames
                          :vocabulary vocabulary))
         (kkc (hachee.kkc:make-kkc
               :cost-fn (hachee.kkc.convert.cost-fns:of-word-pron
                         :vocabulary vocabulary
                         :language-model language-model)
               :dictionary dictionary)))
    (call-with-read-input (lambda (stream line)
      (format stream "~A~%" (kkc-eval kkc (as-expr line)))))))
