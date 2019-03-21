(defpackage :senn.win.keys
  (:use :cl)
  (:export :key
           :make-key
           :key-code))
(in-package :senn.win.keys)

(defstruct key
  code)

(defmacro defkey (name code)
  `(progn
     (defun ,name (k)
       (= (key-code k) ,code))
     (export ',name)))

(defkey enter-p 13)
(defkey space-p 32)
