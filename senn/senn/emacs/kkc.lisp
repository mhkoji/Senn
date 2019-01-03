(defpackage :senn.emacs.kkc
  (:use :cl :senn.op)
  (:export :kkc-eval))
(in-package :senn.emacs.kkc)

(defun kkc-eval (kkc expr)
  (ecase (expr-op expr)
    (:convert
     ;; {"op": "convert", "args": {"text": "あおぞらぶんこ"}}
     (let ((words (senn.kkc:convert kkc (expr-arg expr "text")
                   :1st-boundary-index
                   (expr-arg expr "1st-boundary-index"))))
       (jsown:to-json
        (mapcar (lambda (word)
                  (jsown:new-js
                    ("form" (senn.kkc:word-form word))
                    ("pron" (senn.kkc:word-pron word))))
                words))))
    (:lookup
     ;; {"op": "lookup", "args": {"text": "あお"}}
     (let ((words (senn.kkc:lookup kkc (expr-arg expr "text"))))
       (jsown:to-json
        (mapcar (lambda (word)
                  (jsown:new-js
                    ("form" (senn.kkc:word-form word))))
                words))))))
