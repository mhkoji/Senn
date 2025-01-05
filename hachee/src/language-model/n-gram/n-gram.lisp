(defpackage :hachee.language-model.n-gram
  (:use :cl)
  (:export :train
           :get-count
           :transition-probability
           :model
           :class-model
           :make-classifier
           :sentence-log-probability))
(in-package :hachee.language-model.n-gram)

(defun each-n-gram-subseq (BOS tokens EOS n callback)
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

(labels ((list-subseqs (BOS tokens EOS n)
           (let ((result nil))
             (each-n-gram-subseq BOS tokens EOS n
                                 (lambda (subseq) (push subseq result)))
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
  (assert (equal (list-subseqs 'A '(1 2 3 4) 'A 2)
                 '((A  1) (A)
                   (1  2) (1) NIL
                   (2  3) (2) NIL
                   (3  4) (3) NIL
                   (4  A) (4) NIL
                          (A) NIL)))
  (assert (equal (list-subseqs 'BOS '(1 2 3 4) 'EOS 3)
                 '((BOS BOS   1) (BOS BOS)
                   (BOS   1   2) (BOS   1) (BOS)
                   (  1   2   3) (  1   2) (  1) NIL
                   (  2   3   4) (  2   3) (  2) NIL
                   (  3   4 EOS) (  3   4) (  3) NIL
                                 (  4 EOS) (  4) NIL
                                           (EOS) NIL))))
;;;

(defmacro inchash (key hash)
  `(incf (gethash ,key ,hash 0)))

(defstruct freq
  (hash (make-hash-table :test #'equal)))

(defun freq-get (freq tokens)
  (gethash tokens (freq-hash freq)))

(defun freq-inc (freq tokens)
  (inchash tokens (freq-hash freq)))

(defun conditional-probability (freq token history-tokens)
  (alexandria:when-let
      ((numer (freq-get freq (append history-tokens (list token))))
       (denom (freq-get freq history-tokens)))
    (/ numer denom)))

;;;

(defgeneric train (model sentence-provider &key BOS EOS))

(defmethod train ((model t) (list list) &key BOS EOS)
  (labels ((next ()
             (pop list)))
    (train model #'next :BOS BOS :EOS EOS)))

(defgeneric transition-probability (model token history-tokens))

;; N-gram model is implemented as a little application of freq.
;; An n-gram language model provides the functions of:
;; - counting the n-gram tokens in a sentence
;; - computing the probability of an event w_1, ..., w_{n-1} to w_n
(defclass model ()
  ((freq
    :initform (make-freq)
    :reader model-freq)
   (weights
    :initarg :weights
    :initform nil
    :reader model-weights)))

(defmethod initialize-instance :after ((model model) &key)
  (with-slots (weights) model
    (when (not weights)
      (setf weights (list 0.8d0 0.2d0)))))

(defun model-n (model)
  (length (model-weights model)))

(defun get-count (model tokens)
  (freq-get (model-freq model) tokens))

(defun add-counts (model tokens &key BOS EOS)
  (let ((inc-count
         (alexandria:curry #'freq-inc (model-freq model))))
    (each-n-gram-subseq BOS tokens EOS (model-n model) inc-count)))

(defun interpolated-probability (model token history-tokens)
  (assert (= (length history-tokens) (1- (model-n model))))
  (let ((freq (model-freq model))
        (weights (model-weights model)))
    (loop for weight in weights

          for sub-history-tokens = history-tokens
              then (cdr sub-history-tokens)

          for prob = (conditional-probability
                      freq token sub-history-tokens)

          when prob sum (* weight prob))))

(assert
 (let ((model (make-instance 'model)))
   (add-counts model '(a b b a c) :BOS 'BOS :EOS 'EOS)
   (and (= (interpolated-probability model 'b '(a))
           (+ (* 0.2d0 2/6) ;; b
              (* 0.8d0 1/2) ;; b | a
              ))
        (= (interpolated-probability model 'a '(BOS))
           (+ (* 0.2d0 2/6) ;; a
              (* 0.8d0 1)   ;; a | BOS
              )))))

(assert
 (let ((model (make-instance 'model :weights '(0.1d0 0.2d0 0.7d0))))
   (add-counts model '(a b b a b) :BOS 'BOS :EOS 'EOS)
   (= (interpolated-probability model 'b '(a b))
      (+ (* 0.1d0 3/6)   ;; b
         (* 0.2d0 1/3)   ;; b | b
         (* 0.7d0 1/2)   ;; b | a b
         ))))

(defmacro do-sentence ((s sentence-provider) &body body)
  `(loop for ,s = (funcall ,sentence-provider)
         while ,s do (progn ,@body)))

(defmethod train ((model model) (sentence-provider function) &key BOS EOS)
  (do-sentence (sentence sentence-provider)
    (let ((tokens (hachee.language-model.corpus:sentence-tokens
                   sentence)))
      (add-counts model tokens :BOS BOS :EOS EOS)))
  model)

(defmethod transition-probability ((model model)
                                   (token t)
                                   (history-tokens list))
  (interpolated-probability model token history-tokens))


(defstruct classifier to-class-map)

(defun class-token (classifier x)
  (or (gethash x (classifier-to-class-map classifier))
      (error "Unknown token: ~A" x)))

(defclass class-model (model)
  ((classifier
    :initarg :classifier
    :reader class-model-classifier)
   (token-freq
    :initarg :token-freq
    :initform (make-hash-table :test #'equal)
    :reader class-model-token-freq)
   (class-token-freq
    :initarg :class-token-freq
    :initform (make-hash-table :test #'equal)
    :reader class-model-class-token-freq)))

(defmethod train ((model class-model) (sentence-provider function)
                  &key BOS EOS)
  (let* ((classifier (class-model-classifier model))
         (token-freq (class-model-token-freq model))
         (class-token-freq (class-model-class-token-freq model))
         (class-BOS (class-token classifier BOS))
         (class-EOS (class-token classifier EOS)))
    (do-sentence (sentence sentence-provider)
      (let ((sentence-tokens
             (hachee.language-model.corpus:sentence-tokens
              sentence)))
        (dolist (token sentence-tokens)
          (inchash token token-freq))
        (inchash EOS token-freq)
        (let ((sentence-class-tokens
               (mapcar (lambda (x)
                         (class-token classifier x))
                       sentence-tokens)))
          (dolist (class-token sentence-class-tokens)
            (inchash class-token class-token-freq))
          (inchash class-EOS class-token-freq)
          (add-counts model sentence-class-tokens
                      :BOS class-BOS :EOS class-EOS)))))
  model)

(defun class-interpolated-probability (class-model
                                       token
                                       history-tokens)
  (let ((classifier (class-model-classifier class-model)))
    (let ((class-token
           (class-token classifier token))
          (history-class-tokens
           (mapcar (alexandria:curry #'class-token classifier)
                   history-tokens)))
      (interpolated-probability class-model
                                class-token
                                history-class-tokens))))

(defun class-token->token-probability (class-model token)
  (let ((classifier (class-model-classifier class-model))
        (token-freq (class-model-token-freq class-model))
        (class-token-freq (class-model-class-token-freq class-model)))
    (let ((class-token (class-token classifier token)))
      (let ((token-count (gethash token token-freq)))
        (if token-count
            (/ token-count (gethash class-token class-token-freq))
            0)))))

(defmethod transition-probability ((model class-model)
                                   (token t)
                                   (history-tokens list))
  (* (class-interpolated-probability model token history-tokens)
     (class-token->token-probability model token)))

;;;

(defun sentence-log-probability (model sentence &key BOS EOS)
  (let ((n (model-n model)))
    (let ((bos-tokens (make-list (1- n) :initial-element BOS))
          (eos-tokens (list EOS))
          (sentence-tokens (hachee.language-model.corpus:sentence-tokens
                            sentence)))
      (let ((tokens (append bos-tokens sentence-tokens eos-tokens)))
        (loop for curr-index from (1- n) to (1- (length tokens))
              for p = (transition-probability
                       model
                       (nth curr-index tokens)
                       (subseq tokens (- curr-index (1- n)) curr-index))
              when (= p 0) return (- #xFFFF)
              sum (log p))))))
