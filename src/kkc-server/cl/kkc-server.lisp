(defpackage :hachee.kkc-server
  (:use :cl)
  (:export :main))
(in-package :hachee.kkc-server)

(defun call-with-read-input (callback)
  (loop for line = (read-line *standard-input* nil nil)
        while line do (funcall callback *standard-output* line)))

(defun kkc-eval (kkc expr)
  (destructuring-bind (op &rest args) expr
    (ecase op
      (:convert
       ;; (:convert "あおぞらぶんこ")
       (hachee.kkc:convert kkc (car args))))))

(defun main (pathnames)
  (let* ((dictionary (hachee.kkc:build-dictionary pathnames))
         (vocabulary (hachee.kkc:build-vocabulary pathnames))
         (language-model (hachee.kkc:build-language-model
                          pathnames
                          :vocabulary vocabulary))
         (kkc (hachee.kkc:make-kkc
               :converter (hachee.kkc.converters.word-pron:make-converter
                           :vocabulary vocabulary
                           :language-model language-model)
               :dictionary dictionary)))
    (call-with-read-input
     (lambda (stream line)
       (format stream "~A~%" (kkc-eval kkc (read-from-string line)))))))
