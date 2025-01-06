(defpackage :hachee.language-model.ngram.probability
  (:use :cl)
  (:export :get-count
           :inc-count
           :add-ngram-counts
           :interpolated-probability))
(in-package :hachee.language-model.ngram.probability)

(defgeneric get-count (freq tokens))
(defgeneric inc-count (freq tokens))

(defun each-ngram-subseq (tokens BOS EOS n callback)
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

(defun add-ngram-counts (freq tokens BOS EOS n)
  (each-ngram-subseq tokens BOS EOS n (lambda (tokens)
                                        (inc-count freq tokens))))


(defun conditional-probability (freq token history-tokens)
  (alexandria:when-let
      ((numer (get-count freq (append history-tokens (list token))))
       (denom (get-count freq history-tokens)))
    (/ numer denom)))

(defun interpolated-probability (freq weights token history-tokens)
  (loop for weight in weights

        for sub-history-tokens = history-tokens
            then (cdr sub-history-tokens)

        for prob = (conditional-probability
                    freq token sub-history-tokens)

        when prob sum (* weight prob)))


(labels ((list-subseqs (tokens BOS EOS n)
           (let ((result nil))
             (each-ngram-subseq tokens BOS EOS n
                                (lambda (subseq) (push subseq result)))
             (nreverse result))))
  (assert (equal (list-subseqs '(1 2 3 4) 'BOS 'EOS 1)
                 '((  1) NIL
                   (  2) NIL
                   (  3) NIL
                   (  4) NIL
                   (EOS) NIL)))
  (assert (equal (list-subseqs '(1 2 3 4) 'BOS 'EOS 2)
                 '((BOS   1) (BOS)
                   (  1   2) (  1) NIL
                   (  2   3) (  2) NIL
                   (  3   4) (  3) NIL
                   (  4 EOS) (  4) NIL
                             (EOS) NIL)))
  (assert (equal (list-subseqs '(1 2 3 4) 'A 'A 2)
                 '((A  1) (A)
                   (1  2) (1) NIL
                   (2  3) (2) NIL
                   (3  4) (3) NIL
                   (4  A) (4) NIL
                          (A) NIL)))
  (assert (equal (list-subseqs '(1 2 3 4) 'BOS 'EOS 3)
                 '((BOS BOS   1) (BOS BOS)
                   (BOS   1   2) (BOS   1) (BOS)
                   (  1   2   3) (  1   2) (  1) NIL
                   (  2   3   4) (  2   3) (  2) NIL
                   (  3   4 EOS) (  3   4) (  3) NIL
                                 (  4 EOS) (  4) NIL
                                           (EOS) NIL))))

;; TODO: Make tests
(defmethod get-count ((freq hash-table) tokens)
  (gethash tokens freq))

(defmethod inc-count ((freq hash-table) tokens)
  (incf (gethash tokens freq 0)))

(assert
 (let ((freq (make-hash-table :test #'equal))
       (weights '(0.8d0 0.2d0)))
   (add-ngram-counts freq '(a b b a c) 'BOS 'EOS 2)
   (and (= (interpolated-probability freq weights 'b '(a))
           (+ (* 0.2d0 2/6) ;; b
              (* 0.8d0 1/2) ;; b | a
              ))
        (= (interpolated-probability freq weights 'a '(BOS))
           (+ (* 0.2d0 2/6) ;; a
              (* 0.8d0 1)   ;; a | BOS
              )))))

(assert
 (let ((freq (make-hash-table :test #'equal))
       (weights '(0.1d0 0.2d0 0.7d0) ))
   (add-ngram-counts freq '(a b b a b) 'BOS 'EOS 3)
   (= (interpolated-probability freq weights 'b '(a b))
      (+ (* 0.1d0 3/6)   ;; b
         (* 0.2d0 1/3)   ;; b | b
         (* 0.7d0 1/2)   ;; b | a b
         ))))
