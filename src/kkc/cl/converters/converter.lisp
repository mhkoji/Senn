(defpackage :hachee.kkc.converters.converter
  (:use :cl)
  (:export :cost))
(in-package :hachee.kkc.converters.converter)

(defgeneric cost (converter word-pron-string history-word-pron-string-list))
