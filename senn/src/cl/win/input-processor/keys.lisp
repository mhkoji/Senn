(defpackage :senn.win.input-processor.keys
  (:use :cl)
  (:export :key
           :make-key
           :key-code))
(in-package :senn.win.input-processor.keys)

(defstruct key
  code)

(defmacro defkey (name code)
  `(progn
     (defun ,name (k)
       (= (key-code k) ,code))
     (export ',name)))

(defkey backspace-p 8)
(defkey enter-p 13)
(defkey space-p 32)
(defkey left-p 37)
(defkey up-p 38)
(defkey right-p 39)
(defkey down-p 40)
