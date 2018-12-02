(defpackage :hachee.input-method.kkc-server
  (:use :cl :hachee.input-method.op)
  (:export :enter-loop))
(in-package :hachee.input-method.kkc-server)

(defun kkc-eval (kkc expr)
  (ecase (expr-op expr)
    (:convert
     ;; {"op": "convert", "args": {"text": "あおぞらぶんこ"}}
     (let ((words (hachee.kkc:convert kkc (expr-arg expr "text")
                   :1st-boundary-index
                   (expr-arg expr "1st-boundary-index"))))
       (jsown:to-json
        (mapcar (lambda (word)
                  (jsown:new-js
                    ("form" (hachee.kkc.word:word-form word))
                    ("pron" (hachee.kkc.word:word-pron word))))
                words))))
    (:lookup
     ;; {"op": "lookup", "args": {"text": "あお"}}
     (let ((words (hachee.kkc:lookup kkc (expr-arg expr "text"))))
       (jsown:to-json
        (mapcar (lambda (word)
                  (jsown:new-js
                    ("form" (hachee.kkc.word:word-form word))))
                words))))))

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
