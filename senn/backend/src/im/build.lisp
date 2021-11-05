(defpackage :senn.im.build
  (:use :cl)
  (:export :ime))
(in-package :senn.im.build)

(defclass ime (senn.im:ime
               senn.im.mixin:convert-kkc
               senn.im.mixin:lookup-kkc
               senn.im.mixin:predict-katakana)
  ())
               

(defun ime (kkc)
  (make-instance 'ime
                 :convert-kkc-impl kkc
                 :lookup-kkc-impl kkc))
