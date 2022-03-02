(defpackage :hachee.kkc.lookup
  (:use :cl)
  (:export :item-form
           :item-origin

           :lookup-score-fn
           :lookup-word-dictionary
           :lookup-char-dictionary
           :execute))
(in-package :hachee.kkc.lookup)

(defgeneric item-form (item))
(defgeneric item-origin (item))

(defgeneric execute (lookup pronunciation &key prev next))
