(defpackage :senn.im.kkc
  (:use :cl)
  (:export :convert
           :lookup))
(in-package :senn.im.kkc)

(defgeneric convert (kkc pron &key 1st-boundary-index))

(defgeneric lookup (kkc pron &key prev next))
