(defpackage :hachee.kkc.impl.markov.in-dict
  (:use :cl)
  (:export :entry-form
           :entry-cost
           :entry-token
           :entry-origin
           :make-entry
           :list-entries
           :make-in-dict))
(in-package :hachee.kkc.impl.markov.in-dict)

(defstruct in-dict
  hash)

(defstruct entry
  form cost token origin)

(defun list-entries (dict pron)
  (gethash pron (in-dict-hash dict)))
