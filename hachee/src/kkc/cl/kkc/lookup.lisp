(defpackage :hachee.kkc.lookup
  (:use :cl)
  (:export :execute))
(in-package :hachee.kkc.lookup)

(defun execute (pronunciation &key dictionaries)
  (let ((result-words nil))
    (dolist (dict dictionaries)
      (dolist (word (hachee.kkc.word.dictionary:lookup dict pronunciation))
        (pushnew word result-words
                 :test #'equal
                 :key #'hachee.kkc.word:word->key)))
    (nreverse result-words)))

