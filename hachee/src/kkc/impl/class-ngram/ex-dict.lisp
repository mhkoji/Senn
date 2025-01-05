(defpackage :hachee.kkc.impl.class-ngram.ex-dict
  (:use :cl)
  (:export :entry-form
           :entry-unk-log-probability
           :make-entry
           :list-entries
           :make-ex-dict))
(in-package :hachee.kkc.impl.class-ngram.ex-dict)

(defstruct ex-dict
  hash)

(defstruct entry
  form unk-log-probability)

(defun list-entries (dict pron)
  (gethash pron (ex-dict-hash dict)))
