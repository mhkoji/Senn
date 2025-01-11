(defpackage :hachee.corpus.data
  (:use :cl)
  (:export :set-data-path
           :word-pron-utf8-pathnames))
(in-package :hachee.corpus.data)

(defvar *data-pathname* nil)

(defun set-data-path (path)
  (setq *data-pathname* path))

(defun word-pron-utf8-pathnames ()
  (uiop:directory-files
   (merge-pathnames "aozora/word-pron-utf8/" *data-pathname*)))
