(defpackage :hachee.kkc.word
  (:use :cl)
  (:export :make-word
           :word-pron
           :word-form
           :word->key))
(in-package :hachee.kkc.word)

(defun make-word (&key form pron)
  (concatenate 'string form "/" pron))

(defun word-form (word)
  (first (cl-ppcre:split "/" word)))

(defun word-pron (word)
  (second (cl-ppcre:split "/" word)))

(defun word->key (word)
  word)
