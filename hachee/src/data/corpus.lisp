(defpackage :hachee.data.corpus
  (:use :cl)
  (:export :set-data-path
           :word-pron-utf8-pathnames))
(in-package :hachee.data.corpus)

(defvar *data-pathname* nil)

(defun set-data-path (path)
  (setq *data-pathname* path))

(defun word-pron-utf8-pathnames ()
  (uiop:directory-files
   (merge-pathnames "corpus/aozora/word-pron-utf8/" *data-pathname*)))
