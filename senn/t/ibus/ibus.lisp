(defpackage :senn.t.ibus
  (:use :cl)
  (:export :add-tests
           :run))
(in-package :senn.t.ibus)

(defmacro add-tests (name &rest syms)
  `(progn
     (fiveam:in-suite* ,name :in :senn.t.ibus)
     ,@(mapcar (lambda (sym)
                 `(fiveam:test ,sym (,sym :test fiveam:is)))
               syms)))

(fiveam:def-suite :senn.t.ibus)

(defun run ()
  (fiveam:run! :senn.t.ibus))
