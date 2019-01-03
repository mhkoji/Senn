(defpackage :hachee.input-method.emacs.kkc
  (:use :cl :hachee.input-method.op)
  (:export :kkc-eval))
(in-package :hachee.input-method.emacs.kkc)

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
