;; Provide the interface of kkc
(defpackage :senn.kkc
  (:use :cl :hachee.kkc)
  (:export :convert
           :lookup
           :word-pron
           :word-form))
