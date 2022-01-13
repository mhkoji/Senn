(defpackage :senn.im.kkc
  (:use :cl)
  (:export :segment-form
           :segment-pron
           :make-segment

           :candidate
           :candidate-form
           :make-candidate

           :convert
           :list-candidates))
(in-package :senn.im.kkc)

(defstruct segment pron form)

(defstruct candidate form)

(defgeneric convert (kkc pron &key 1st-boundary-index))

(defgeneric list-candidates (kkc index))
