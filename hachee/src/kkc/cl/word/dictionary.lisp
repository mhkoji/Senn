(defpackage :hachee.kkc.word.dictionary
  (:use :cl)
  (:export :add
           :lookup
           :size
           :dictionary
           :list-all-words
           :make-dictionary
           :save-dictionary
           :load-dictionary))
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

(defun list-all-words (dictionary)
  (loop for words in (alexandria:hash-table-values
                      (dictionary-hash dictionary))
        nconc (copy-list words)))

(defun size (dictionary)
  (hash-table-size (dictionary-hash dictionary)))

(defun save-dictionary (dict stream)
  (print (list :hash
               (alexandria:hash-table-alist (dictionary-hash dict)))
         stream)
  (values))

(defun load-dictionary (stream)
  (let ((list (read stream)))
    (make-dictionary :hash
                     (alexandria:alist-hash-table
                      (getf list :hash) :test #'equal))))
