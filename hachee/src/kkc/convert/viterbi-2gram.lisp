(defpackage :hachee.kkc.convert.viterbi-2gram
  (:use :cl)
  (:export :execute
           :node
           :node-entry))
(in-package :hachee.kkc.convert.viterbi-2gram)

(defstruct node entry score-so-far prev-node)

(defun compute-score (score-fn entry prev-entry)
  (funcall score-fn entry prev-entry))

(defun connect-by-max-score (score-fn nodes prev-nodes &key print-p)
  (when prev-nodes
    (dolist (node nodes)
      (dolist (prev-node prev-nodes)
        (when (null (node-score-so-far prev-node))
          (error "Node without score: ~A" prev-node))
        (let ((score-so-far (+ (node-score-so-far prev-node)
                               (compute-score score-fn
                                              (node-entry node)
                                              (node-entry prev-node)))))
          (when print-p
            (print (list (node-entry node)
                         (node-score-so-far node))))
          (when (or (null (node-score-so-far node))
                    (< (node-score-so-far node) score-so-far))
            (setf (node-prev-node node) prev-node)
            (setf (node-score-so-far node) score-so-far))))))
  (values))

(defun execute (pronunciation &key begin-entry
                                   end-entry
                                   score-fn
                                   list-entries-fn
                                   1st-boundary-index)
  (when (and (numberp 1st-boundary-index)
             (<= 8 1st-boundary-index))
    ;; Can not create an unknown word of length longer than 7
    (setq 1st-boundary-index 7))
  (let ((table (make-hash-table))
        (length (length pronunciation))
        (begin-node (make-node :entry begin-entry
                               :score-so-far 0
                               :prev-node nil))
        (end-node (make-node :entry end-entry)))
    ;; Create table
    (push (cons -1 (list begin-node))
          (gethash 0 table))
    (push (cons length (list end-node))
          (gethash (1+ length) table))
    (loop for end from 1 to length do
      (loop for start from 0 below end do
        (when (or (not 1st-boundary-index)
                  (and (= start 0)
                       (= end 1st-boundary-index))
                  (<= 1st-boundary-index start))
          (let* ((sub-pron (subseq pronunciation start end))
                 (nodes (mapcar (lambda (ent)
                                  (make-node :entry ent))
                                (funcall list-entries-fn sub-pron))))
            (push (cons start nodes) (gethash end table))))))
    ;; DP
    (loop for end from 1 to (1+ length) do
      (loop for (start . nodes) in (gethash end table) do
        (loop for (prev-start . prev-nodes) in (gethash start table) do
          (connect-by-max-score score-fn nodes prev-nodes :print-p nil))))
    (labels ((backtrack (node acc)
               (if (null (node-prev-node node))
                   acc
                   (backtrack (node-prev-node node)
                              (cons node acc)))))
      (let ((entries (mapcar #'node-entry
                             ;; skip end-entry
                             (backtrack (node-prev-node end-node) nil))))
        (values entries end-node table)))))
