(defpackage :senn.im.kkc.wrapper
  (:use :cl)
  (:export :wrapper))
(in-package :senn.im.kkc.wrapper)

(defclass wrapper ()
  ((kkc
    :initarg :kkc
    :reader wrapper-kkc)))

(defmethod senn.im.kkc:convert ((kkc wrapper) pron &key 1st-boundary-index)
  (senn.im.kkc:convert (wrapper-kkc kkc) pron
           :1st-boundary-index 1st-boundary-index))

(defmethod senn.im.kkc:list-candidates ((kkc wrapper) pron)
  (senn.im.kkc:list-candidates (wrapper-kkc kkc) pron))
