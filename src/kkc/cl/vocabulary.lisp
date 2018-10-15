(defpackage :hachee.kkc.vocabulary
  (:use :cl)
  (:export :add-str
           :to-int
           :to-int-or-unk
           :to-str
           :make-vocabulary
           :+UNK+ :+BOS+ :+EOS+))
(in-package :hachee.kkc.vocabulary)

(defparameter +UNK+ "__UNK__")
(defparameter +BOS+ "__BOS__")
(defparameter +EOS+ "__EOS__")

(defclass vocabulary ()
  ((size :initform 0
         :accessor vocabulary-size)
   (to-int-map :initform (make-hash-table :test #'equal)
               :accessor vocabulary-to-int-map)
   (to-str-map :initform (make-hash-table :test #'equal)
               :accessor vocabulary-to-str-map)))

(defun add-str (vocab string)
  (with-accessors ((size vocabulary-size)
                   (to-int-map vocabulary-to-int-map)
                   (to-str-map vocabulary-to-str-map)) vocab
    (when (not (gethash string to-int-map))
      (setf (gethash size to-str-map) string)
      (setf (gethash string to-int-map) size)
      (incf size))))

(defun make-vocabulary ()
  (let ((vocab (make-instance 'vocabulary)))
    (add-str vocab +UNK+)
    (add-str vocab +BOS+)
    (add-str vocab +EOS+)
    vocab))

(defun to-int (vocab str)
  (with-accessors ((map vocabulary-to-int-map)) vocab
    (gethash str map)))

(defun to-int-or-unk (vocab str)
  (or (to-int vocab str) (to-int vocab +UNK+)))

  (defun to-str (vocab token)
  (or (gethash token (vocabulary-to-str-map vocab))
      (error "stringize: unknown token ~A" token)))

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
