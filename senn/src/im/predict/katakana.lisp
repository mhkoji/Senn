(defpackage :senn.im.predict.katakana
  (:use :cl)
  (:export :predict))
(in-package :senn.im.predict.katakana)

(defclass predict () ())

(defmethod senn.im.ime:predict append ((mixin predict) (string string))
  (list (senn.ja:hiragana->katakana string)))
