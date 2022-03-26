(defpackage :hachee.kkc.impl.markov.ex-dict
  (:use :cl)
  (:export :entry-form
           :entry-cost
           :make-entry
           :make-ex-dict
           :list-entries))
(in-package :hachee.kkc.impl.markov.ex-dict)

(defstruct ex-dict
  hash)

(defstruct entry
  form cost)

(defun list-entries (dict pron)
  (gethash pron (ex-dict-hash dict)))
