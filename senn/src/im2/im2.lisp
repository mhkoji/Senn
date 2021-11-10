;;; Under construction
(defpackage :senn.im2
  (:use :cl)
  (:export :ime

           :state
           :make-state

           :chars
           :input-char))
(in-package :senn.im2)

(defclass ime () ())

(defstruct state)

(defgeneric chars (state))

(defgeneric input-char (ime state char))
