(defpackage :hachee.kkc.impl.mirror.int-str
  (:use :cl)
  (:export :+UT+
           :+BT+
           :to-int
           :int-str-size
           :make-int-str))
(in-package :hachee.kkc.impl.mirror.int-str)

(defparameter +UT+ 0) ;; unknown token
(defparameter +BT+ 1) ;; boundary token

(defstruct int-str str->int)

(defun to-int (int-str str)
  (or (gethash str (int-str-str->int int-str)) +UT+))

(defun int-str-size (int-str)
  (hash-table-count (int-str-str->int int-str)))
