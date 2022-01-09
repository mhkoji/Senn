(defpackage :senn.im.predict.katakana
  (:use :cl)
  (:export :predictor))
(in-package :senn.im.predict.katakana)

(defclass predictor () ())

(defmethod senn.im.predict:execute append ((mixin predictor) (string string))
  (list (senn.ja:hiragana->katakana string)))
