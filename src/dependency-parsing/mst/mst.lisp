(defpackage :hachee.dependency-parsing.mst
  (:use :cl :hachee.dependency-parsing)
  (:export :train :parse))
(in-package :hachee.dependency-parsing.mst)

(defstruct model
  string-feature-extractor
  string-feature-indexer
  feature-weight-vector
  left-to-right-p)

(defun train (sentences extractor
              &key (cutoff 0)
                   (left-to-right-p t))
  (let ((training-set (cl-lr:make-empty-training-set)))
    (dolist (sentence sentences)
      (let ((length (length (sentence-rows sentence))))
        (loop for i from 1 below length
              for target = (row-head (elt (sentence-rows sentence) i))
              when (/= target -1) do
          (let ((begin (if left-to-right-p (1+ i) 0)))
            (let ((vectors
                   (loop for j from begin below length
                         when (/= j i)
                         collect (cl-lr:make-feature-vector
                                  (funcall extractor sentence i j))))
                  (gold-index
                   (if left-to-right-p
                       (- target begin)
                       (- target (if (< i target) 1 0)))))
              (cl-lr:training-set-append! training-set
                                          vectors gold-index))))))
    ;; WIP: Index features
    (let ((weight-vector
           (cl-lr:fit training-set
                      (make-instance 'cl-lr.solver:liblbfgs))))
      (make-model :string-feature-extractor extractor
                  :string-feature-indexer nil
                  :feature-weight-vector weight-vector
                  :left-to-right-p left-to-right-p))))

(defun generate-score-hash (model sentence)
  (let ((N               (sentence-length sentence))
        (extractor       (model-string-feature-extractor model))
        (indexer         (model-string-feature-indexer model))
        (weight-vector   (model-feature-weight-vector model))
        (left-to-right-p (model-left-to-right-p model))
        (score-hash (make-hash-table :test #'equal)))
    (loop for i from 1 below N do
      (let ((sum 0)
            (begin (if left-to-right-p (+ i 1) 0)))
        (loop for j from begin below N
          when (/= j i) do
            (let ((feature-vector
                   (cl-lr:make-feature-vector
                    (funcall extractor sentence i j)
                    indexer)))
              (let ((score (exp (cl-lr:feature-vector-innner
                                 feature-vector weight-vector))))
                (setf (hachee.algorithm.mst:get-score score-hash
                                                      j i)
                      score)
                (incf sum score))))
        (loop for j from begin below N
          when (/= j i) do
            (setf (hachee.algorithm.mst:get-score score-hash j i)
                  (/ (hachee.algorithm.mst::get-score score-hash j i)
                     sum)))))
    score-hash))

(defun parse (model sentence)
  (let* ((N (sentence-length sentence))
         (vertices (loop for i from 0 below N collect i))
         (score-hash (generate-score-hash model sentence)))
    (let ((edges (hachee.algorithm.mst::chu-liu-edmonds
                  N
                  score-hash
                  vertices)))
      (loop for (j i) in edges do
        (setf (row-head (elt (sentence-rows sentence) i)) j)))
    sentence))
