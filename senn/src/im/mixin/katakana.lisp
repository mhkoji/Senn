(defpackage :senn.im.mixin.katakana
  (:use :cl)
  (:export :predict))
(in-package :senn.im.mixin.katakana)

(defclass predict () ())

(defmethod senn.im:predict append ((mixin predict) (string string))
  (list (hachee.ja:hiragana->katakana string)))
