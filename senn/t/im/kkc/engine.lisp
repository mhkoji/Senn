(defpackage :senn.t.im.kkc.engine
  (:use :cl))
(in-package :senn.t.im.kkc.engine)

(defmethod senn.im.kkc.request:send-line ((agent function) line)
  (funcall agent line))

(defmacro test-invalid-json-then-pron (&key test)
  `(let ((segs (senn.im.kkc.request:convert
                (lambda (line) "[{")
                "test")))
     (,test (= (length segs) 1))
     (let ((seg (car segs)))
       (,test (string= (senn.im.kkc:segment-pron seg) "test"))
       (,test (= (length (senn.im.kkc:segment-candidates seg)) 1))
       (let ((cand (car (senn.im.kkc:segment-candidates seg))))
         (,test (string= (senn.im.kkc:candidate-form cand) "test"))))))

(fiveam:def-suite :senn.t.im.kkc.engine :in :senn.t)
(fiveam:in-suite :senn.t.im.kkc.engine)

(fiveam:test test-invalid-json-then-pron
  (test-invalid-json-then-pron :test fiveam:is))
