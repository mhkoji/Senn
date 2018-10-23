(defpackage :hachee.kkc.dictionary
  (:use :cl)
  (:export :make-dictionary
           :add
           :lookup))
(in-package :hachee.kkc.dictionary)

(defstruct dictionary
  (hash (make-hash-table :test #'equal)))

(defun add (dictionary pron thing)
  (pushnew thing (gethash pron (dictionary-hash dictionary))
           :test #'equal))

(defun lookup (dictionary pron)
  (gethash pron (dictionary-hash dictionary)))
