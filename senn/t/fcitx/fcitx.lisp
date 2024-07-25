(defpackage :senn.t.fcitx
  (:use :cl)
  (:export :add-tests
           :char-key
           :space-key))
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

(defun char-key (char)
  (senn.fcitx.keys:make-key :sym (char-code char) :state 0))

(defun space-key ()
  (senn.fcitx.keys:make-key :sym 32 :state 0))
