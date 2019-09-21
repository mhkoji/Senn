(defpackage :senn.emacs.kkc
  (:use :cl :hachee.ipc.op)
  (:export :kkc-eval))
(in-package :senn.emacs.kkc)

(defun convert-origin (origin)
  (case origin
    (:extended-dictionary "EX")
    (:tankan-dictionary "TK")
    (:unknown-word "UW")
    (t "")))

(defun kkc-eval (kkc expr)
  (ecase (expr-op expr)
    (:convert
     ;; {"op": "convert", "args": {"text": "あおぞらぶんこ"}}
     (let ((nodes (senn.kkc:convert-to-nodes kkc (expr-arg expr "text")
                   :1st-boundary-index
                   (expr-arg expr "1st-boundary-index"))))
       (jsown:to-json
        (mapcar (lambda (n)
                  (let ((word (hachee.kkc.convert:node-word n))
                        (origin (hachee.kkc.convert:node-word-origin n)))
                    (jsown:new-js
                      ("form" (senn.kkc:word-form word))
                      ("pron" (senn.kkc:word-pron word))
                      ("origin" (convert-origin origin)))))
                nodes))))
    (:lookup
     ;; {"op": "lookup", "args": {"text": "あお"}}
     (let ((items (senn.kkc:lookup-items kkc (expr-arg expr "text"))))
       (jsown:to-json
        (mapcar (lambda (item)
                  (jsown:new-js
                    ("form" (hachee.kkc.lookup:item-form item))
                    ("origin" (convert-origin
                               (hachee.kkc.lookup:item-origin item)))))
                items))))))
