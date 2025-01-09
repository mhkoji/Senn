(defpackage :hachee.language-model.ngram.freq
  (:use :cl)
  (:export :freq
           :make-freq
           :with-add-counts
           :do-ngram-count))
(in-package :hachee.language-model.ngram.freq)

(defstruct freq
  (hash (make-hash-table :test #'equal)))

(defun freq-get (freq tokens)
  (gethash tokens (freq-hash freq)))

(defun freq-inc (freq tokens)
  (incf (gethash tokens (freq-hash freq) 0)))

(defmethod hachee.language-model.ngram.probability:get-count
    ((freq freq) (tokens list))
  (freq-get freq tokens))

(defmethod hachee.language-model.ngram.probability:inc-count
    ((freq freq) (tokens list))
  (freq-inc freq tokens))

(defmacro with-add-counts ((add-counts freq &key n BOS EOS)
                           &body body)
  (let ((sym-n (gensym))
        (sym-BOS (gensym))
        (sym-EOS (gensym))
        (sym-freq (gensym)))
    `(let ((,sym-n ,n)
           (,sym-BOS ,BOS)
           (,sym-EOS ,EOS)
           (,sym-freq ,freq))
       (labels ((,add-counts (tokens)
                  (hachee.language-model.ngram.probability:add-ngram-counts
                   ,sym-freq ,sym-n tokens ,sym-BOS ,sym-EOS)))
         (progn ,@body)))))

(defmacro do-ngram-count ((tokens count freq) &body body)
  (let ((sym-freq (gensym)))
    `(let ((,sym-freq ,freq))
       (let ((tokens-list
              (sort (alexandria:hash-table-keys
                     (freq-hash ,sym-freq))
                    (lambda (tokens1 tokens2)
                      (let ((n1 (length tokens1))
                            (n2 (length tokens2)))
                        (if (/= n1 n2)
                            (< n1 n2)
                            (loop for t1 in tokens1
                                  for t2 in tokens2
                                  when (< t1 t2) do (return t)
                                  when (< t2 t1) do (return nil)
                                  finally (progn t))))))))
         (dolist (,tokens tokens-list)
           (let ((,count (freq-get ,sym-freq ,tokens)))
             ,@body))))))
