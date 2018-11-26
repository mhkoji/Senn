(defpackage :hachee.kkc.word.vocabulary
  (:use :cl)
  (:export :add
           :to-int
           :to-int-or-unk
           :+UNK+ :+BOS+ :+EOS+
           :make-vocabulary)
  (:import-from :hachee.kkc.word
                :make-word
                :word-pron
                :word-form
                :word->key))
(in-package :hachee.kkc.word.vocabulary)

(defparameter +UNK+
  (make-word :pron "__UNK__" :form ""))

(defparameter +BOS+
  (make-word :pron "__BOS__" :form ""))

(defparameter +EOS+
  (make-word :pron "__EOS__" :form ""))

(defclass vocabulary ()
  ((size :initform 0
         :accessor vocabulary-size)
   (to-int-map :initform (make-hash-table :test #'equal)
               :accessor vocabulary-to-int-map)))

(defun add (vocab word)
  (with-accessors ((size vocabulary-size)
                   (to-int-map vocabulary-to-int-map)) vocab
    (let ((key (word->key word)))
      (when (not (gethash key to-int-map))
        (setf (gethash key to-int-map) size)
        (incf size)))))

(defun make-vocabulary ()
  (let ((vocab (make-instance 'vocabulary)))
    (add vocab +UNK+)
    (add vocab +BOS+)
    (add vocab +EOS+)
    vocab))

(defun to-int (vocab word)
  (gethash (word->key word) (vocabulary-to-int-map vocab)))

(defun to-int-or-unk (vocab word)
  (or (to-int vocab word) (to-int vocab +UNK+)))


#+nil
(defun make-vocabulary-with-unk (vocab pathnames &key (overlap 2))
  (let ((appear (make-hash-table :test #'equal)))
    (dolist (pathname pathnames)
      (let ((curr-appear (make-hash-table :test #'equal)))
        (with-each-line (line pathname)
          (dolist (token-str (cl-ppcre:split line " "))
            (setf (gethash token-str curr-appear) t)))
        (loop for token-str being the hash-key of curr-appear
              do (when (<= overlap (inchash token-str appear))
                   (add-str vocab token-str))))))
  vocab)
