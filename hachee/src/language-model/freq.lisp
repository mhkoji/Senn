(defpackage :hachee.language-model.freq
  (:use :cl)
  (:export :make-freq
           :inc-count
           :get-count
           :conditional-probability))
(in-package :hachee.language-model.freq)

(defmacro inchash (key hash)
  `(incf (gethash ,key ,hash 0)))

(defstruct freq
  (hash (make-hash-table :test #'equal)))

(defmacro inchash (key hash)
  `(incf (gethash ,key ,hash 0)))

(defun inc-count (freq tokens)
  (inchash tokens (freq-hash freq)))

(defun get-count (freq tokens)
  (gethash tokens (freq-hash freq)))
