(defpackage :hachee.language-model.n-gram
  (:use :cl)
  (:import-from :alexandria
                :curry
                :when-let)
  (:import-from :hachee.language-model
                :sentence-tokens)
  (:import-from :hachee.language-model.freq
                :inc-count
                :get-count)
  (:export :model
           :class-model
           :make-classifier
           :train
           :transition-probability
           :sentence-log-probability
           :save-model
           :load-model))
(in-package :hachee.language-model.n-gram)

(defun conditional-probability (freq token history-tokens)
  (when-let ((numer (get-count freq (append history-tokens (list token))))
             (denom (get-count freq history-tokens)))
    (/ numer denom)))

(defun each-n-gram-subseq (BOS tokens EOS n callback)
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
  (assert (equal (list-subseqs 'BOS '(1 2 3 4) 'EOS 3)
                 '((BOS BOS   1) (BOS BOS)
                   (BOS   1   2) (BOS   1) (BOS)
                   (  1   2   3) (  1   2) (  1) NIL
                   (  2   3   4) (  2   3) (  2) NIL
                   (  3   4 EOS) (  3   4) (  3) NIL
                                 (  4 EOS) (  4) NIL
                                           (EOS) NIL))))

;; N-gram model is implemented as a little application of freq.
;; An n-gram language model provides the functions of:
;; - counting the n-gram tokens in a sentence
;; - computing the probability of an event w_1, ..., w_{n-1} to w_n
(defclass model ()
  ((freq
    :initarg :freq
    :initform (hachee.language-model.freq:make-empty)
    :reader model-freq)
   (weights
    :initarg :weights
    :initform (list 0.2d0 0.8d0)
    :reader model-weights)))

(defun model-n (model)
  (length (model-weights model)))

(defun count-n-grams (model tokens &key BOS EOS)
  (let ((inc-count (curry #'inc-count (model-freq model))))
    (each-n-gram-subseq BOS tokens EOS (model-n model) inc-count)))

(defun interpolated-probability (model token history-tokens)
  (assert (= (length history-tokens) (1- (model-n model))))
  (let ((sub-history-tokens-list
         (loop repeat (model-n model)

               for sub-history-begin
                   from (length history-tokens) downto 0

               for sub-history-tokens
                   = (subseq history-tokens sub-history-begin)

               collect sub-history-tokens)))
    (reduce #'+
            (mapcar (lambda (weight sub-history-tokens)
                      (* weight
                         (or (conditional-probability (model-freq model)
                                                      token
                                                      sub-history-tokens)
                             0)))
                    (model-weights model) sub-history-tokens-list))))

(assert
 (let ((model (make-instance 'model)))
   (count-n-grams model '(a b b a c) :BOS 'BOS :EOS 'EOS)
   (and (= (interpolated-probability model 'b '(a))
           (+ (* 0.2d0 2/6) ;; b
              (* 0.8d0 1/2) ;; b | a
              ))
        (= (interpolated-probability model 'a '(BOS))
           (+ (* 0.2d0 2/6) ;; a
              (* 0.8d0 1)   ;; a | EOS
              )))))

(assert
 (let ((model (make-instance 'model :weights '(0.1d0 0.2d0 0.7d0))))
   (count-n-grams model '(a b b a b) :BOS 'BOS :EOS 'EOS)
   (= (interpolated-probability model 'b '(a b))
      (+ (* 0.1d0 3/6)   ;; b
         (* 0.2d0 1/3)   ;; b | b
         (* 0.7d0 1/2)   ;; b | a b
         ))))


(defclass class-model (model)
  ((classifier
    :initarg :classifier
    :reader class-model-classifier)
   (token-freq
    :initform (make-hash-table :test #'equal)
    :initarg :token-freq
    :reader class-model-token-freq)
   (class-token-freq
    :initform (make-hash-table :test #'equal)
    :initarg :class-token-freq
    :reader class-model-class-token-freq)))

(defstruct classifier to-class-map)


(defgeneric train (model sentences &key BOS EOS))

(defgeneric transition-probability (model token history-tokens))

(defgeneric save-model (model stream))

(defgeneric load-model (type stream))

(defmethod train ((model model) (sentences list) &key BOS EOS)
  (dolist (sentence sentences)
    (let ((sentence-tokens (sentence-tokens sentence)))
      (count-n-grams model sentence-tokens :BOS BOS :EOS EOS)))
  model)

(defmethod transition-probability ((model model)
                                   (token t)
                                   (history-tokens list))
  (interpolated-probability model token history-tokens))

(defmethod save-model ((model model) stream)
  (print (list :freq (hachee.language-model.freq:to-alist
                      (model-freq model))
               :weights (model-weights model))
         stream)
  (values))

(defmethod load-model ((type (eql 'model)) stream)
  (let ((list (read stream)))
    (make-instance 'model
                   :freq (hachee.language-model.freq:make-by-alist
                          (getf list :freq))
                   :weights (getf list :weights))))


(defun class-token (classifier x)
  (or (gethash x (classifier-to-class-map classifier))
      (error "Unknown token: ~A" x)))

(defmacro inchash (key hash)
  `(incf (gethash ,key ,hash 0)))

(defmethod train ((model class-model) (sentences list) &key BOS EOS)
  (let ((classifier (class-model-classifier model))
        (token-freq (class-model-token-freq model))
        (class-token-freq (class-model-class-token-freq model)))
    (let ((bos-class-token (class-token classifier BOS))
          (eos-class-token (class-token classifier EOS)))
      (dolist (sentence sentences)
        (let* ((sentence-tokens (sentence-tokens sentence))
               (sentence-class-tokens
                (mapcar (lambda (x)
                          (class-token classifier x))
                        sentence-tokens)))
          (dolist (token sentence-tokens)
            (inchash token token-freq))
          (inchash EOS token-freq)
          (dolist (class-token sentence-class-tokens)
            (inchash class-token class-token-freq))
          (inchash eos-class-token class-token-freq)
          (count-n-grams model sentence-class-tokens
                         :BOS bos-class-token
                         :EOS eos-class-token)))))
  model)

(defun class-interpolated-probability (class-model
                                       token
                                       history-tokens)
  (let ((classifier (class-model-classifier class-model)))
    (let ((class-token
           (class-token classifier token))
          (history-class-tokens
           (mapcar (curry #'class-token classifier) history-tokens)))
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

(defmethod save-model ((model class-model) stream)
  (call-next-method)
  (print (list :classifier
               (alexandria:hash-table-alist
                (classifier-to-class-map
                 (class-model-classifier model)))
               :token-freq
               (alexandria:hash-table-alist
                (class-model-token-freq model))
               :class-token-freq
               (alexandria:hash-table-alist
                (class-model-class-token-freq model)))
         stream)
  (values))

(defmethod load-model ((type (eql 'class-model)) stream)
  (let ((model (load-model 'model stream))
        (plist (read stream)))
    (change-class model 'class-model
                  :classifier
                  (make-classifier
                   :to-class-map
                   (alexandria:alist-hash-table
                    (getf plist :classifier) :test #'equal))
                  :token-freq
                  (alexandria:alist-hash-table
                   (getf plist :token-freq) :test #'equal)
                  :class-token-freq
                  (alexandria:alist-hash-table
                   (getf plist :class-token-freq) :test #'equal))))

(defun sentence-log-probability (model sentence &key BOS EOS)
  (let ((n (model-n model)))
    (let ((bos-tokens (make-list (1- n) :initial-element BOS))
          (eos-tokens (list EOS)))
      (let ((tokens (append bos-tokens
                            (sentence-tokens sentence)
                            eos-tokens)))
        (loop for curr-index from (1- n) to (1- (length tokens))
              for p = (transition-probability
                       model
                       (nth curr-index tokens)
                       (subseq tokens (- curr-index (1- n)) curr-index))
              when (= p 0) return -10000
              sum (log p))))))
