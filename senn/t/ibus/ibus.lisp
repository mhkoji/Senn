(defpackage :senn.t.ibus
  (:use :cl)
  (:export :add-tests))
(in-package :senn.t.ibus)

(fiveam:def-suite :senn.t.ibus :in :senn.t)

(defmacro add-tests (name &rest syms)
  (let ((full-name (intern (concatenate 'string
                            "SENN.T.IBUS." (string-upcase name))
                           :keyword)))
    `(progn
       (fiveam:def-suite ,full-name :in :senn.t.ibus)
       ,@(mapcar (lambda (sym)
                   `(fiveam:def-test ,sym (:suite ,full-name)
                      (,sym :test fiveam:is)))
                 syms))))
