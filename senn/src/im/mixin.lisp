(defpackage :senn.im.mixin
  (:use :cl)
  (:export :convert-kkc
           :lookup-kkc
           :predict-katakana
           :predict-prefix))
(in-package :senn.im.mixin)

(defclass convert-kkc ()
  ((impl
    :initarg :convert-kkc-impl
    :reader convert-kkc-impl)))

(defmethod senn.im:convert ((mixin convert-kkc) (pron string)
                            &key 1st-boundary-index)
  (senn.im.kkc:convert (convert-kkc-impl mixin) pron
           :1st-boundary-index 1st-boundary-index))

(defclass lookup-kkc ()
  ((impl
    :initarg :lookup-kkc-impl
    :reader lookup-kkc-impl)))

(defmethod senn.im:lookup ((mixin lookup-kkc) (pron string)
                           &key prev next)
  (senn.im.kkc:lookup (lookup-kkc-impl mixin) pron :next next :prev prev))

(defclass predict-katakana () ())

(defmethod senn.im:predict append ((mixin predict-katakana)
                                   (string string))
  (list (hachee.ja:hiragana->katakana string)))

(defclass predict-prefix ()
  ((dictionary
    :initarg :predict-prefix-dictionary
    :reader predict-prefix-dictionary)))

(defmethod senn.im:predict append ((mixin predict-prefix)
                                   (string string))
  (senn.im.prefix-dictionary:predict
   (predict-prefix-dictionary mixin) string))
