(defpackage :hachee.kkc.lookup
  (:use :cl)
  (:export :execute))
(in-package :hachee.kkc.lookup)

(defun execute (pronunciation &key dictionary)
  (hachee.kkc.dictionary:lookup dictionary pronunciation))
