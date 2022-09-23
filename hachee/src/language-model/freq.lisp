(defpackage :hachee.language-model.freq
  (:use :cl)
  (:export :inc-count
           :get-count

           :make-empty
           :make-by-alist
           :to-alist
           :count-n-grams))
(in-package :hachee.language-model.freq)

(defmacro inchash (key hash)
  `(incf (gethash ,key ,hash 0)))

(defstruct freq
  (hash (make-hash-table :test #'equal)))

(defun inc-count (freq tokens)
  (inchash tokens (freq-hash freq)))

(defun get-count (freq tokens)
  (gethash tokens (freq-hash freq)))


(defun make-empty ()
  (make-freq))

(defun make-by-alist (alist)
  (make-freq :hash (alexandria:alist-hash-table alist :test #'equal)))

(defun to-alist (freq)
  (alexandria:hash-table-alist (freq-hash freq)))
