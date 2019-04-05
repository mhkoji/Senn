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

(defkey space-p 32)
(defkey enter-p 13)
(defkey backspace-p -1)
(defkey left-p -1)
(defkey down-p -1)
(defkey right-p -1)
(defkey up-p -1)
