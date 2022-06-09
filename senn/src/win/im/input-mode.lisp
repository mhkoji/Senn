(defpackage :senn.win.im.input-mode
  (:use :cl)
  (:export :+hiragana+
           :+direct+
           :mode=
           :mode-case))
(in-package :senn.win.im.input-mode)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +hiragana+
    :hiragana)

  (defvar +direct+
    :direct))

(defun mode= (m1 m2)
  (eql m1 m2))

(defmacro mode-case (mode &body clauses)
  (assert (null (set-difference
                 (mapcar #'first clauses)
                 (list +hiragana+ +direct+))))
  `(ecase ,mode
     (#.+hiragana+
      ,@(cdr (assoc #.+hiragana+ clauses)))
     (#.+direct+
      ,@(cdr (assoc #.+direct+ clauses)))))
