(defpackage :hachee.language-model.n-gram
  (:use :cl)
  (:import-from :alexandria
                :curry
                :when-let)
  (:export :make-n-gram
           :n-gram-n
           :add-counts
           :interpolated-probability))
(in-package :hachee.language-model.n-gram)

(defstruct freq
  (hash (make-hash-table :test #'equal)))

(defmacro inchash (key hash)
  `(incf (gethash ,key ,hash 0)))

(defun inc-count (freq tokens)
  (inchash tokens (freq-hash freq)))

(defun get-count (freq tokens)
  (gethash tokens (freq-hash freq)))


;;; N-gram in the context of language modeling provides the functions of:
;;; - counting the n-gram tokens in a sentence
;;; - computing the probability of an event w_1, ..., w_{n-1} to w_n
;;; N-gram is implemented as a little application of freq.
(defstruct n-gram
  (freq (make-freq))
  (weights '(0.2d0 0.8d0)))

(defun call-with-n-gram-subseq (BOS tokens EOS n callback)
  (let ((bos-tokens (make-list (1- n) :initial-element BOS))
        (eos-tokens (list EOS)))
    (let ((extended-tokens (append bos-tokens tokens eos-tokens)))
      (let ((length (length extended-tokens)))
        (dotimes (start length)
          (loop for diff from (min n (- length start))
                              downto 0
                for end = (+ start diff)
                for subseq = (subseq extended-tokens start end)
                do (funcall callback subseq)
                while (not (eq (car (last subseq)) BOS))))))))

(labels ((list-subseqs (BOS tokens EOS n)
           (let ((result nil))
             (call-with-n-gram-subseq BOS tokens EOS n
                                      (lambda (subseq)
                                        (push subseq result)))
             (nreverse result))))
  (assert (equal (list-subseqs 'BOS '(1 2 3 4) 'EOS 1)
                 '((  1) NIL
                   (  2) NIL
                   (  3) NIL
                   (  4) NIL
                   (EOS) NIL)))
  (assert (equal (list-subseqs 'BOS '(1 2 3 4) 'EOS 2)
                 '((BOS   1) (BOS)
                   (  1   2) (  1) NIL
                   (  2   3) (  2) NIL
                   (  3   4) (  3) NIL
                   (  4 EOS) (  4) NIL
                             (EOS) NIL)))
  (assert (equal (list-subseqs 'BOS '(1 2 3 4) 'EOS 3)
                 '((BOS BOS   1) (BOS BOS)
                   (BOS   1   2) (BOS   1) (BOS)
                   (  1   2   3) (  1   2) (  1) NIL
                   (  2   3   4) (  2   3) (  2) NIL
                   (  3   4 EOS) (  3   4) (  3) NIL
                                 (  4 EOS) (  4) NIL
                                           (EOS) NIL))))

(defun n-gram-n (n-gram)
  (length (n-gram-weights n-gram)))

(defun add-counts (n-gram tokens &key BOS EOS)
  (call-with-n-gram-subseq BOS tokens EOS (n-gram-n n-gram)
                           (curry #'inc-count (n-gram-freq n-gram))))


(defun conditional-probability (freq token history-tokens)
  (when-let ((numer (get-count freq (append history-tokens (list token))))
             (denom (get-count freq history-tokens)))
    (/ numer denom)))

(defun interpolated-probability (n-gram token history-tokens)
  (assert (= (length history-tokens) (1- (n-gram-n n-gram))))
  (let ((sub-history-tokens-list
         (loop repeat (n-gram-n n-gram)

               for sub-history-begin
                   from (length history-tokens) downto 0

               for sub-history-tokens
                   = (subseq history-tokens sub-history-begin)

               collect sub-history-tokens)))
    (reduce #'+
            (mapcar (lambda (weight sub-history-tokens)
                      (* weight
                         (or (conditional-probability (n-gram-freq n-gram)
                                                      token
                                                      sub-history-tokens)
                             0)))
                    (n-gram-weights n-gram) sub-history-tokens-list))))

(assert
 (let ((n-gram (make-n-gram)))
   (add-counts n-gram '(a b b a c) :BOS 'BOS :EOS 'EOS)
   (and (= (interpolated-probability n-gram 'b '(a))
           (+ (* 0.2d0 2/6) ;; b
              (* 0.8d0 1/2) ;; b | a
              ))
        (= (interpolated-probability n-gram 'a '(BOS))
           (+ (* 0.2d0 2/6) ;; a
              (* 0.8d0 1)   ;; a | EOS
              )))))

(assert
 (let ((n-gram (make-n-gram :weights '(0.1d0 0.2d0 0.7d0))))
   (add-counts n-gram '(a b b a b) :BOS 'BOS :EOS 'EOS)
   (= (interpolated-probability n-gram 'b '(a b))
      (+ (* 0.1d0 3/6)   ;; b
         (* 0.2d0 1/3)   ;; b | b
         (* 0.7d0 1/2)   ;; b | a b
         ))))
