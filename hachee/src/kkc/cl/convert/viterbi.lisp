(defpackage :hachee.kkc.convert.viterbi
  (:use :cl)
  (:export :execute
           :node
           :node-entry))
(in-package :hachee.kkc.convert.viterbi)

(defstruct node entry score-so-far prev-node)

(defun compute-score (score-fn entry prev-entry)
  (funcall score-fn entry prev-entry))

(defun update-prev-node (node score-fn prev-nodes &key print-p)
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
        (setf (node-score-so-far node) score-so-far))))
  (values))

(defun backtrack (node acc)
  (if (null (node-prev-node node))
      acc
      (backtrack (node-prev-node node)
                 (cons node acc))))

(defun add-entries (table end prev-entries nodes)
  (push (list prev-entries nodes)
        (gethash end table)))

(defun get-entries (table end)
  (gethash end table))

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
        (length (length pronunciation)))
    ;; Create table
    (add-entries table 0 nil (list (make-node :entry begin-entry
                                              :score-so-far 0
                                              :prev-node nil)))
    (loop for end from 1 to length do
      (loop for start from 0 below end do
        (when (or (not 1st-boundary-index)
                  (and (= start 0)
                       (= end 1st-boundary-index))
                  (<= 1st-boundary-index start))
          (let ((sub-pron (subseq pronunciation start end)))
            (let ((nodes (mapcar (lambda (ent)
                                   (make-node :entry ent))
                                 (funcall list-entries-fn sub-pron)))
                  (prev-entries (get-entries table start)))
              ;; Add the nodes to the table
              (add-entries table end prev-entries nodes))))))
    ;; DP
    (loop for end from 1 to length do
      (loop for (prev-entries nodes) in (get-entries table end) do
        (dolist (prev-entry prev-entries)
          (let ((prev-nodes (second prev-entry)))
            (when prev-nodes
              (dolist (node nodes)
                (update-prev-node node score-fn prev-nodes
                                  :print-p nil)))))))
    (let ((end-node (make-node :entry end-entry)))
      (loop for (prev-entries nodes) in (get-entries table length)
            do (update-prev-node end-node score-fn nodes
                                 :print-p nil))
      ;; skip end-entry
      (values (mapcar #'node-entry
                      (backtrack (node-prev-node end-node) nil))
              end-node
              table))))
