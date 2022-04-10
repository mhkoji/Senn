(defpackage :senn.t.fcitx
  (:use :cl)
  (:export :add-tests
           :run))
(in-package :senn.t.fcitx)

(defmacro add-tests (name &rest syms)
  `(progn
     (fiveam:in-suite* ,name :in :senn.t.fcitx)
     ,@(mapcar (lambda (sym)
                 `(fiveam:test ,sym (,sym :test fiveam:is)))
               syms)))

(fiveam:def-suite :senn.t.fcitx)

(defun run ()
  (fiveam:run! :senn.t.fcitx))
