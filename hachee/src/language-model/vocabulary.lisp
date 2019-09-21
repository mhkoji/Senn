(defpackage :hachee.language-model.vocabulary
  (:use :cl)
  (:export :to-int
           :to-int-or-nil
           :to-int-or-unk
           :add-new
           :+UNK+ :+BOS+ :+EOS+
           :vocabulary
           :make-vocabulary
           :save-vocabulary
           :load-vocabulary))
(in-package :hachee.language-model.vocabulary)

(defparameter +UNK+ "<UNK>")

(defparameter +BOS+ "<BOS>")

(defparameter +EOS+ "<EOS>")

(defclass vocabulary ()
  ((size :initarg :size
         :initform 0
         :accessor vocabulary-size)
   (to-int-map :initarg :to-int-map
               :initform (make-hash-table :test #'equal)
               :accessor vocabulary-to-int-map)))

(defun to-int-or-nil (vocab key)
  (gethash key (vocabulary-to-int-map vocab)))

(defun to-int (vocab key)
  (let ((int (to-int-or-nil vocab key)))
    (assert int)
    int))

(defun to-int-or-unk (vocab key)
  (or (to-int-or-nil vocab key) (to-int vocab +UNK+)))

(defun add-new (vocab key)
  (with-accessors ((size vocabulary-size)
                   (to-int-map vocabulary-to-int-map)) vocab
    (when (not (gethash key to-int-map))
      (setf (gethash key to-int-map) size)
      (incf size))))

(defun make-vocabulary ()
  (let ((vocab (make-instance 'vocabulary)))
    (add-new vocab +UNK+)
    (add-new vocab +BOS+)
    (add-new vocab +EOS+)
    vocab))

(defun save-vocabulary (vocab stream)
  (print (list :to-int-map
               (alexandria:hash-table-alist
                (vocabulary-to-int-map vocab)))
         stream)
  (values))

(defun load-vocabulary (stream)
  (let ((list (read stream)))
    (let ((map (alexandria:alist-hash-table
                (getf list :to-int-map)
                :test #'equal)))
      (make-instance 'vocabulary
                     :size (hash-table-count map)
                     :to-int-map map))))
