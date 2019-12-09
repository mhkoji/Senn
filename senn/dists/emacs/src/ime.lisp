(defpackage :senn.emacs.ime
  (:use :cl :hachee.ipc.op)
  (:export :process))
(in-package :senn.emacs.ime)

(defun convert-origin (origin)
  (case origin
    (:extended-dictionary "EX")
    (:tankan-dictionary "TK")
    (:unknown-word "UW")
    (t "")))

(defun process (ime expr)
  (ecase (expr-op expr)
    (:convert
     ;; {"op": "convert", "args": {"text": "あおぞらぶんこ"}}
     (let ((segments
            (senn.im:convert ime (expr-arg expr "text")
                             :1st-boundary-index
                             (expr-arg expr "1st-boundary-index"))))
       (jsown:to-json
        (mapcar (lambda (seg)
                  (let ((cand (car (senn.segment:segment-candidates seg))))
                    (jsown:new-js
                      ("pron" (senn.segment:segment-pron seg))
                      ("form" (senn.segment:candidate-form cand))
                      ("origin" (convert-origin
                                 (senn.segment:candidate-origin cand))))))
                segments))))
    (:lookup
     ;; {"op": "lookup", "args": {"text": "あお"}}
     (let ((candidates
            (senn.im:lookup ime (expr-arg expr "text"))))
       (jsown:to-json
        (mapcar (lambda (cand)
                  (jsown:new-js
                    ("form" (senn.segment:candidate-form cand))
                    ("origin" (convert-origin
                               (senn.segment:candidate-origin cand)))))
                candidates))))))
