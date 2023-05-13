(defpackage :senn.t.im.kkc.engine
  (:use :cl))
(in-package :senn.t.im.kkc.engine)

(defmethod senn.im.kkc.request:send-line ((agent function) line)
  (funcall agent line))

(defmacro test-invalid-json-then-pron (&key test)
  `(let ((segs (senn.im.kkc.request:convert
                (lambda (line)
                  (declare (ignore line))
                  "[{")
                "test")))
     (,test (= (length segs) 1))
     (let ((seg (car segs)))
       (,test (string= (senn.im.kkc:segment-pron seg) "test"))
       (,test (= (length (senn.im.kkc:segment-candidates seg)) 1))
       (let ((cand (car (senn.im.kkc:segment-candidates seg))))
         (,test (string= (senn.im.kkc:candidate-form cand) "test"))))))


(defstruct engine-store get-fn restart-fn)

(defmethod senn.im.kkc.engine::store-get-engine ((store engine-store))
  (funcall (engine-store-get-fn store)))

(defmethod senn.im.kkc.engine::store-restart-engine ((store engine-store))
  (funcall (engine-store-restart-fn store)))

(defmacro test-error-then-restart (&key test)
  `(let ((restart-called-count 0))
     (let ((segs (senn.im.kkc:convert
                  (make-instance
                   'senn.im.kkc.engine:kkc
                   :engine-store
                   (make-engine-store
                    :get-fn (lambda ()
                              (error "Error"))
                    :restart-fn (lambda ()
                                  (incf restart-called-count))))
                  "test")))
       (,test (= (length segs) 1))
       (let ((seg (car segs)))
         (,test (string= (senn.im.kkc:segment-pron seg) "test"))
         (,test (= (length (senn.im.kkc:segment-candidates seg)) 1))
         (let ((cand (car (senn.im.kkc:segment-candidates seg))))
           (,test (string= (senn.im.kkc:candidate-form cand) "test")))))
     (,test (= restart-called-count 1))))

(fiveam:def-suite :senn.t.im.kkc.engine :in :senn.t)
(fiveam:in-suite :senn.t.im.kkc.engine)

(fiveam:test test-invalid-json-then-pron
  (test-invalid-json-then-pron :test fiveam:is))

(fiveam:test test-error-then-restart
  (test-error-then-restart :test fiveam:is))
