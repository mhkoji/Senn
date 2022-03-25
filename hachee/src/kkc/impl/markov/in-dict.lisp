(defpackage :hachee.kkc.impl.markov.in-dict
  (:use :cl)
  (:export :entry-form
           :entry-cost
           :entry-token
           :make-entry
           :list-entries
           :list-prons
           :make-in-dict))
(in-package :hachee.kkc.impl.markov.in-dict)

(defstruct in-dict
  hash)

(defstruct entry
  form cost token)

(defun list-entries (dict pron)
  (gethash pron (in-dict-hash dict)))

(defun list-prons (dict)
  (let ((hash (in-dict-hash dict)))
    (loop for pron being the hash-key of hash collect pron)))
