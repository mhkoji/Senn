(defpackage :hachee.kkc.word
  (:use :cl)
  (:shadow :char=)
  (:export :make-char
           :char-pron
           :char-form
           :char=
           :char->key
           :make-word
           :word-pron
           :word-form
           :word=
           :word->key
           :word->pron-chars))
(in-package :hachee.kkc.word)

(defun make-char (&key form pron)
  (concatenate 'string form "/" pron))

(defun char-form (char)
  (first (cl-ppcre:split "/" char)))

(defun char-pron (char)
  (second (cl-ppcre:split "/" char)))

(defun char= (char1 char2)
  (string= char1 char2))

(defun char->key (char)
  char)


(defun make-word (&key form pron)
  (concatenate 'string form "/" pron))

(defun word-form (word)
  (first (cl-ppcre:split "/" word)))

(defun word-pron (word)
  (second (cl-ppcre:split "/" word)))

(defun word= (word1 word2)
  (string= word1 word2))

(defun word->key (word)
  word)

(defun word->pron-chars (word)
  (loop for ch across (word-pron word)
        collect (make-char :form (string ch)
                           :pron (string ch))))
