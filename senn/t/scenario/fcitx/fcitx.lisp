(defpackage :senn.t.scenario.fcitx
  (:use :cl)
  (:export :add-tests
           :run))
(in-package :senn.t.scenario.fcitx)

(defmacro add-tests (name &rest syms)
  `(progn
     (fiveam:in-suite* ,name :in :senn.t.scenario.fcitx)
     ,@(mapcar (lambda (sym)
                 `(fiveam:test ,sym (,sym :test fiveam:is)))
               syms)))

(fiveam:def-suite :senn.t.scenario.fcitx)

(defun run ()
  (fiveam:run! :senn.t.scenario.fcitx))
