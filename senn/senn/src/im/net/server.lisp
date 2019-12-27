(defpackage :senn.im.net.server
  (:use :cl :hachee.ipc.op)
  (:export :read-message
           :send-message
           :loop-handling-request)
  (:import-from :alexandria
                :when-let))
(in-package :senn.im.net.server)

(defgeneric read-message (conn))
(defgeneric send-message (conn msg))

(defun expr->word (expr)
  (hachee.kkc.word:make-word
   :pron (jsown:val expr "pron")
   :form (jsown:val expr "form")))

(defun handle-request (expr ime)
  (ecase (expr-op expr)
    (:convert
     ;; {"op": "convert", "args": {"text": "あおぞらぶんこ"}}
     (let ((segments
            (senn.im:convert ime (expr-arg expr "text")
             :1st-boundary-index (expr-arg expr "1st-boundary-index"))))
       (jsown:to-json
        (mapcar (lambda (seg)
                  (let ((cand (car (senn.segment:segment-candidates seg))))
                    (jsown:new-js
                      ("pron" (senn.segment:segment-pron seg))
                      ("form" (senn.segment:candidate-form cand))
                      ("origin" (senn.segment:candidate-origin cand)))))
                segments))))
    (:lookup
     ;; {"op": "lookup", "args": {"text": "あお"}}
     (let ((candidates
            (senn.im:lookup ime (expr-arg expr "text")
             :prev (let ((prev (expr-arg expr "prev")))
                     (when prev (expr->word prev)))
             :next (let ((next (expr-arg expr "next")))
                     (when next (expr->word next))))))
       (jsown:to-json
        (mapcar (lambda (cand)
                  (jsown:new-js
                    ("form" (senn.segment:candidate-form cand))
                    ("origin" (senn.segment:candidate-origin cand))))
                candidates))))
    (:predict
     (let ((strings
            (senn.im:predict ime (expr-arg expr "text"))))
       (jsown:to-json strings)))))

(defun loop-handling-request (ime client-conn)
  (when-let ((msg (read-message client-conn)))
    (let ((expr (hachee.ipc.op:as-expr msg)))
      (send-message client-conn (handle-request expr ime)))
    (loop-handling-request ime client-conn)))
