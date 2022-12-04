(defpackage :hachee.kkc.impl.mirror.ex-dict
  (:use :cl)
  (:export :entry-form
           :entry-cost
           :make-entry
           :list-entries
           :make-ex-dict))
(in-package :hachee.kkc.impl.mirror.ex-dict)

(defstruct ex-dict
  hash)

(defstruct entry
  form cost)

(defun list-entries (dict pron)
  (gethash pron (ex-dict-hash dict)))
