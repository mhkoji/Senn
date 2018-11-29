(defpackage :hachee.kkc.word.dictionary
  (:use :cl)
  (:export :make-dictionary
           :add
           :lookup))
(in-package :hachee.kkc.word.dictionary)

(defstruct dictionary
  (hash (make-hash-table :test #'equal)))

(defun add (dictionary word)
  (let ((pron (hachee.kkc.word:word-pron word)))
    (when (string/= pron "")
      (pushnew word (gethash pron (dictionary-hash dictionary))
               :test #'equal))))

(defun lookup (dictionary pron)
  (gethash pron (dictionary-hash dictionary)))
