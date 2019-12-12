(defpackage :hachee.kkc.word.dictionary
  (:use :cl)
  (:export :lookup
           :list-all
           :size
           :dictionary
           :make-dictionary
           :save-dictionary
           :load-dictionary
           :add-word
           :contains-word-p
           :add-char))
(in-package :hachee.kkc.word.dictionary)

(defstruct dictionary
  (hash (make-hash-table :test #'equal)))

(defun lookup (dictionary pron)
  (gethash pron (dictionary-hash dictionary)))

(defun list-all (dictionary)
  (loop for items in (alexandria:hash-table-values
                      (dictionary-hash dictionary))
        nconc (copy-list items)))

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


(defun add-new (dictionary key item test)
  (pushnew item (gethash key (dictionary-hash dictionary))
           :test test))

(defun add-word (dictionary word)
  (let ((key (hachee.kkc.word:word-pron word)))
    (add-new dictionary key word #'hachee.kkc.word:word=)))

(defun contains-word-p (dictionary word)
  (member word (lookup dictionary (hachee.kkc.word:word-pron word))
          :test #'hachee.kkc.word:word=))

(defun add-char (dictionary char)
  (let ((key (hachee.kkc.word:char-pron char)))
    (add-new dictionary key char #'hachee.kkc.word:char=)))
