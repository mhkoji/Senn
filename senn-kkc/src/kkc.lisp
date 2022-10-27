(defpackage :senn-kkc
  (:use :cl)
  (:export :segment-pron
           :segment-candidates
           :make-segment

           :candidate
           :candidate-form
           :make-candidate

           :convert
           :list-candidates))
(in-package :senn-kkc)

(defstruct segment pron candidates)

(defstruct candidate form)

(defgeneric convert (kkc pron &key 1st-boundary-index))

(defgeneric list-candidates (kkc pron))
