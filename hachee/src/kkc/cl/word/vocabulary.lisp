(defpackage :hachee.kkc.word.vocabulary
  (:use :cl)
  (:shadow :load)
  (:export :add
           :to-int
           :to-int-or-unk
           :+UNK+ :+BOS+ :+EOS+
           :make-vocabulary
           :save
           :load)
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
  ((size :initarg :size
         :initform 0
         :accessor vocabulary-size)
   (to-int-map :initarg :to-int-map
               :initform (make-hash-table :test #'equal)
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


(defun save (vocab stream)
  (print (list :to-int-map
               (alexandria:hash-table-alist (vocabulary-to-int-map vocab)))
         stream)
  (values))

(defun load (stream)
  (let ((list (read stream)))
    (let ((map (alexandria:alist-hash-table (getf list :to-int-map)
                                            :test #'equal)))
      (make-instance 'vocabulary
                     :size (hash-table-count map)
                     :to-int-map map))))
