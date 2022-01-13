(defpackage :senn.im.kkc
  (:use :cl)
  (:export :segment-form
           :segment-pron
           :make-segment

           :candidate
           :candidate-form
           :make-candidate

           :convert
           :lookup))
(in-package :senn.im.kkc)

(defstruct segment pron form)

(defstruct candidate form)

(defgeneric convert (kkc pron &key 1st-boundary-index))

(defgeneric lookup (kkc pron &key prev next))
