(defpackage :senn.t.fcitx
  (:use :cl)
  (:export :add-tests))
(in-package :senn.t.fcitx)

(fiveam:def-suite* :senn.t.fcitx :in :senn.t)

(defmacro add-tests (name &rest syms)
  (let ((full-name (intern (concatenate 'string
                            "SENN.T.FCITX." (string-upcase name))
                           :keyword)))
    `(progn
       (fiveam:def-suite ,full-name :in :senn.t.fcitx)
       ,@(mapcar (lambda (sym)
                   `(fiveam:def-test ,sym (:suite ,full-name)
                      (,sym :test fiveam:is)))
                 syms))))
