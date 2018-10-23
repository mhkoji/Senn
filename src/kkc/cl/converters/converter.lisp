(defpackage :hachee.kkc.converters.converter
  (:use :cl)
  (:export :probability))
(in-package :hachee.kkc.converters.converter)

(defgeneric probability (converter
                         word-pron-string
                         history-word-pron-string-list))
