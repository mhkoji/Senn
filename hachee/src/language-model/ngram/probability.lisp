(defpackage :hachee.language-model.ngram.probability
  (:use :cl)
  (:export :get-count
           :inc-count
           :add-ngram-counts
           :weighted-list))
(in-package :hachee.language-model.ngram.probability)

(defgeneric get-count (freq tokens))
(defgeneric inc-count (freq tokens))

(defun each-ngram-subseq (tokens n BOS EOS callback)
  (let ((bos-tokens (make-list (1- n) :initial-element BOS))
        (eos-tokens (list EOS)))
    (let ((extended-tokens (append bos-tokens tokens eos-tokens)))
      (let ((length (length extended-tokens)))
        (dotimes (start length)
          (loop for diff from (min n (- length start)) downto 0
                for end = (+ start diff) do
            (progn
              (when (not (and (< start (1- n)) (< end (1- n))))
                (funcall callback
                         (subseq extended-tokens start end))))))))))

(defun add-ngram-counts (freq n tokens BOS EOS)
  (each-ngram-subseq tokens n BOS EOS (lambda (tokens)
                                        (inc-count freq tokens))))


(defun conditional-probability (freq token history-tokens)
  (alexandria:when-let
      ((numer (get-count freq (append history-tokens (list token))))
       (denom (get-count freq history-tokens)))
    (/ numer denom)))

(defun weighted-list (freq weights token history-tokens)
  (loop for w in weights

        for sub-history-tokens = history-tokens
            then (cdr sub-history-tokens)

        for p = (or (conditional-probability freq token sub-history-tokens)
                    0)

        collect (* w p)))

(labels ((list-subseqs (tokens n BOS EOS)
           (let ((result nil))
             (each-ngram-subseq tokens n BOS EOS
                                (lambda (subseq) (push subseq result)))
             (nreverse result))))
  (assert (equal (list-subseqs '(1 2 3 4) 1 'BOS 'EOS)
                 '((  1) NIL
                   (  2) NIL
                   (  3) NIL
                   (  4) NIL
                   (EOS) NIL)))
  (assert (equal (list-subseqs '(1 2 3 4) 2 'BOS 'EOS)
                 '((BOS   1) (BOS)
                   (  1   2) (  1) NIL
                   (  2   3) (  2) NIL
                   (  3   4) (  3) NIL
                   (  4 EOS) (  4) NIL
                             (EOS) NIL)))
  (assert (equal (list-subseqs '(1 2 3 4) 2 'A 'A)
                 '((A  1) (A)
                   (1  2) (1) NIL
                   (2  3) (2) NIL
                   (3  4) (3) NIL
                   (4  A) (4) NIL
                          (A) NIL)))
  (assert (equal (list-subseqs '(1 2 3 4) 3 'BOS 'EOS)
                 '((BOS BOS   1) (BOS BOS)
                   (BOS   1   2) (BOS   1) (BOS)
                   (  1   2   3) (  1   2) (  1) NIL
                   (  2   3   4) (  2   3) (  2) NIL
                   (  3   4 EOS) (  3   4) (  3) NIL
                                 (  4 EOS) (  4) NIL
                                           (EOS) NIL))))
