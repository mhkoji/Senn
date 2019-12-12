(defpackage :hachee.kkc.convert
  (:use :cl)
  (:export :execute
           :node-word
           :node-word-origin)
  (:import-from :alexandria
                :when-let))
(in-package :hachee.kkc.convert)

(defstruct node word word-origin score-so-far prev-node)

(defun update-prev-node (node score-fn prev-nodes &key print-p)
  (dolist (prev-node prev-nodes)
    (when (null (node-score-so-far prev-node))
      (error "Node without score: ~A" prev-node))
    (let ((score-so-far (+ (node-score-so-far prev-node)
                           (funcall score-fn
                                    node
                                    prev-node))))
      (when print-p
        (print (list (list (node-word prev-node)
                           (node-word-origin prev-node))
                     (list (node-word node)
                           (node-word-origin node))
                     score-so-far)))
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


(defvar *BOS-node*
  (make-node :word hachee.language-model.vocabulary:+BOS+
             :prev-node nil
             :score-so-far 0))

(defun add-entries (table end prev-entries nodes)
  (push (list prev-entries nodes)
        (gethash end table)))

(defun get-entries (table end)
  (gethash end table))

(defun execute (pronunciation &key score-fn
                                   list-words-fn
                                   1st-boundary-index)
  (when (and (numberp 1st-boundary-index)
             (<= 8 1st-boundary-index))
    ;; Can not create an unknown word of length longer than 7
    (setq 1st-boundary-index 7))
  (let ((table (make-hash-table))
        (length (length pronunciation)))
    ;; Create table
    (add-entries table 0 nil (list *BOS-node*))
    (loop for end from 1 to length do
      (loop for start from 0 below end do
        (when (or (not 1st-boundary-index)
                  (and (= start 0)
                       (= end 1st-boundary-index))
                  (<= 1st-boundary-index start))
          (let ((nodes nil)
                (sub-pron (subseq pronunciation start end)))
            ;; Collect nodes for sub-pron
            (let ((origin-words-alist (funcall list-words-fn sub-pron)))
              (dolist (origin-words origin-words-alist)
                (destructuring-bind (origin . words) origin-words
                  (dolist (word words)
                    (push (make-node :word word :word-origin origin)
                          nodes)))))
            ;; Add the nodes to the table
            (let ((prev-entries (get-entries table start)))
              (add-entries table end prev-entries nodes))))))
    ;; DP
    (loop for end from 1 to length do
      (loop for (prev-entries nodes) in (get-entries table end) do
        (dolist (prev-entry prev-entries)
          (when-let ((prev-nodes (second prev-entry)))
            (dolist (node nodes)
              (update-prev-node node score-fn prev-nodes :print-p nil))))))
    (let ((EOS (make-node :word hachee.language-model.vocabulary:+EOS+)))
      (loop for (prev-entries nodes) in (get-entries table length)
            do (update-prev-node EOS score-fn nodes :print-p nil))
      ;; skip EOS
      (values (backtrack (node-prev-node EOS) nil)
              EOS
              table))))
