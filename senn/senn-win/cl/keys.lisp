(defpackage :senn.win.keys
  (:use :cl)
  (:export :key
           :make-key
           :key-code))
(in-package :senn.win.keys)

(defstruct key
  code)
