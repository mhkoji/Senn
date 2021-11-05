(defpackage :senn.im.build
  (:use :cl)
  (:export :ime))
(in-package :senn.im.build)

(defclass ime (senn.im.predictors:katakana
               senn.im.kkc:convert-mixin
               senn.im.kkc:lookup-mixin
               senn.im:ime) ())

(defun ime (kkc)
  (make-instance 'ime
                 :convert-impl kkc
                 :lookup-impl kkc))
