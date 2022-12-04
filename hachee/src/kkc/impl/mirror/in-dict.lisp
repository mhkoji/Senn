(defpackage :hachee.kkc.impl.mirror.in-dict
  (:use :cl)
  (:export :entry-form
           :entry-cost
           :entry-token
           :make-entry
           :list-entries
           :do-entries
           :make-in-dict))
(in-package :hachee.kkc.impl.mirror.in-dict)

(defstruct in-dict
  hash)

(defstruct entry
  form cost token)

(defun list-entries (dict pron)
  (gethash pron (in-dict-hash dict)))

(defmacro do-entries ((var dict) &body body)
  `(let ((hash (in-dict-hash ,dict)))
     (loop for entries being the hash-value of hash do
       (progn
         (dolist (,var entries)
           (progn ,@body))))))
