(defpackage :hachee.kkc.convert
  (:use :cl :hachee.kkc.word)
  (:export :execute)
  (:import-from :hachee.kkc.word.dictionary
                :lookup)
  (:import-from :hachee.ja
                :hiragana->katakana)
  (:import-from :alexandria
                :when-let))
(in-package :hachee.kkc.convert)

(defun calculate-score (score-fn curr-word prev-words)
  (funcall score-fn curr-word prev-words))


(defstruct node word score-so-far prev-node)

(defun update-prev-node (node score-fn prev-nodes)
  (dolist (prev-node prev-nodes)
    (let ((score-so-far
            (+ (node-score-so-far prev-node)
               (calculate-score score-fn
                                (node-word node)
                                (list (node-word prev-node))))))
      (when (or (null (node-score-so-far node))
                (< (node-score-so-far node) score-so-far))
        (setf (node-prev-node node) prev-node)
        (setf (node-score-so-far node) score-so-far))))
  (values))

(defun backtrack (node acc)
  (if (null (node-prev-node node))
      acc
      (backtrack (node-prev-node node)
                 (cons (node-word node) acc))))


(defvar *BOS-node*
  (make-node :word hachee.kkc.word.vocabulary:+BOS+
             :prev-node nil
             :score-so-far 0))

(defun make-unknown-word (pron)
  (make-word :pron pron :form (hiragana->katakana pron)))


(defun add-entries (table end prev-entries nodes)
  (push (list prev-entries nodes)
        (gethash end table)))

(defun get-entries (table end)
  (gethash end table))

(defun execute (pronunciation &key score-fn dictionary 1st-boundary-index)
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
          (let* ((sub-pron (subseq pronunciation start end))
                 (words (or (lookup dictionary sub-pron)
                            (when (or ;; Length up to 8
                                   (< (- end start) 8)
                                   1st-boundary-index)
                              (list (make-unknown-word sub-pron))))))
            (let ((nodes (mapcar (lambda (w) (make-node :word w))
                                 words))
                  (prev-entries (get-entries table start)))
              (add-entries table end prev-entries nodes))))))
    ;; DP
    (loop for end from 1 to length do
      (loop for (prev-entries nodes) in (get-entries table end) do
        (dolist (prev-entry prev-entries)
          (when-let ((prev-nodes (second prev-entry)))
            (dolist (node nodes)
              (update-prev-node node score-fn prev-nodes))))))
    (let ((EOS (make-node :word hachee.kkc.word.vocabulary:+EOS+)))
      (loop for (prev-entries nodes) in (get-entries table length) do
        (update-prev-node EOS score-fn nodes))
      ;; skip EOS
      (backtrack (node-prev-node EOS) nil))))
