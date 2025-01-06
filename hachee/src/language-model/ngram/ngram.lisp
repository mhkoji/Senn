(defpackage :hachee.language-model.ngram
  (:use :cl)
  (:export :sentence
           :sentence-tokens
           :make-sentence
           :model-add-counts
           :model-probability
           :get-count
           :model
           :class-model
           :make-classifier
           :sentence-log-probability))
(in-package :hachee.language-model.ngram)

(defmacro inchash (key hash)
  `(incf (gethash ,key ,hash 0)))

;;;

(defstruct freq
  (hash (make-hash-table :test #'equal)))

(defun freq-get (freq tokens)
  (gethash tokens (freq-hash freq)))

(defun freq-inc (freq tokens)
  (inchash tokens (freq-hash freq)))

(defmethod hachee.language-model.ngram.probability:get-count
    ((freq freq) (tokens list))
  (freq-get freq tokens))

(defmethod hachee.language-model.ngram.probability:inc-count
    ((freq freq) (tokens list))
  (freq-inc freq tokens))


(defclass mutable-probability-calculation () ())
(defgeneric mutable-probability-calculation-freq (mutable-probability-calculation))
(defgeneric mutable-probability-calculation-weights (mutable-probability-calculation))

(defun mutable-probability-calculation-calculate (calculation token history-tokens)
  (hachee.language-model.ngram.probability:interpolated-probability
   (mutable-probability-calculation-freq calculation)
   (mutable-probability-calculation-weights calculation)
   token
   history-tokens))

(defmacro with-mutable-probability-calculation-add-counts
    ((add-counts calculation &key n BOS EOS)
     &body body)
  (let ((sym-n (gensym))
        (sym-BOS (gensym))
        (sym-EOS (gensym))
        (sym-calc (gensym))
        (sym-freq (gensym)))
    `(let ((,sym-n ,n)
           (,sym-BOS ,BOS)
           (,sym-EOS ,EOS)
           (,sym-calc ,calculation))
       (let ((,sym-freq (mutable-probability-calculation-freq ,sym-calc)))
         (labels ((,add-counts (tokens)
                    (hachee.language-model.ngram.probability:add-ngram-counts
                     ,sym-freq tokens ,sym-BOS ,sym-EOS ,sym-n)))
           (progn ,@body))))))

;;;

(defstruct sentence tokens)

(defgeneric model-add-counts (model sentence-provider &key BOS EOS))

(defgeneric model-probability (model token history-tokens))

(defmethod model-add-counts ((model t) (list list) &key BOS EOS)
  (labels ((next ()
             (pop list)))
    (train model #'next :BOS BOS :EOS EOS)))

(defmacro do-sentence ((s sentence-provider) &body body)
  `(loop for ,s = (funcall ,sentence-provider)
         while ,s do (progn ,@body)))

;;;

(defclass model-mixin (mutable-probability-calculation)
  ((freq
    :initform (make-freq)
    :reader mutable-probability-calculation-freq)
   (weights
    :initform nil
    :initarg :weights
    :reader mutable-probability-calculation-weights)))

(defmethod initialize-instance :after ((model model-mixin) &key)
  (with-slots (weights) model
    (when (not weights)
      (setf weights (list 0.8d0 0.2d0)))))

(defun get-count (model tokens)
  (freq-get (mutable-probability-calculation-freq model) tokens))

(defun model-n (model)
  (length (mutable-probability-calculation-weights model)))

;; ngram model is implemented as a little application of freq.
;; An n-gram language model provides the functions of:
;; - counting the ngram tokens in a sentence
;; - computing the probability of an event w_1, ..., w_{n-1} to w_n
(defclass model (model-mixin)
  ())

(defmethod model-add-counts ((model model) (sentence-provider function)
                             &key BOS EOS)
  (with-mutable-probability-calculation-add-counts
      (add-ngram-counts model
                        :n (model-n model)
                        :BOS BOS
                        :EOS EOS)
    (do-sentence (sentence sentence-provider)
      (add-ngram-counts (sentence-tokens sentence))))
  model)

(defmethod model-probability ((model model)
                              (token t)
                              (history-tokens list))
  (mutable-probability-calculation-calculate model token history-tokens))

;;;

(defstruct classifier to-class-map)

(defun class-token (classifier x)
  (or (gethash x (classifier-to-class-map classifier))
      (error "Unknown token: ~A" x)))

(defclass class-model (model-mixin)
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

(defun add-token-counts (token-freq sentence-tokens EOS)
  (dolist (token sentence-tokens)
    (inchash token token-freq))
  (inchash EOS token-freq))

(defun add-class-token-counts (class-token-freq sentence-class-tokens
                               class-EOS)
  (dolist (class-token sentence-class-tokens)
    (inchash class-token class-token-freq))
  (inchash class-EOS class-token-freq))

(defmethod model-add-counts ((model class-model) (sentence-provider function)
                             &key BOS EOS)
  (let* ((classifier (class-model-classifier model))
         (token-freq (class-model-token-freq model))
         (class-token-freq (class-model-class-token-freq model))
         (class-EOS (class-token classifier EOS)))
    (with-mutable-probability-calculation-add-ngram-counts
        (add-ngram-counts model 
                          :n (model-n model)
                          :BOS (class-token classifier BOS)
                          :EOS class-EOS)
      (do-sentence (sentence sentence-provider)
        (let* ((tokens (sentence-tokens sentence))
               (class-tokens (mapcar (lambda (x)
                                       (class-token classifier x))
                                     tokens)))
          (add-ngram-counts class-tokens)
          (add-token-counts token-freq tokens EOS)
          (add-class-token-counts class-token-freq class-tokens class-EOS)))))
  model)


(defun class-token->token-probability (class-model token)
  (let ((classifier (class-model-classifier class-model))
        (token-freq (class-model-token-freq class-model))
        (class-token-freq (class-model-class-token-freq class-model)))
    (let ((class-token (class-token classifier token)))
      (let ((token-count (gethash token token-freq)))
        (if token-count
            (/ token-count (gethash class-token class-token-freq))
            0)))))

(defun class-interpolated-probability (class-model
                                       token
                                       history-tokens)
  (let ((classifier (class-model-classifier class-model)))
    (let ((class-token
           (class-token classifier token))
          (class-history-tokens
           (mapcar (alexandria:curry #'class-token classifier)
                   history-tokens)))
      (mutable-probability-calculation-calculate class-model
                                                 class-token
                                                 class-history-tokens))))

(defmethod model-probability ((model class-model)
                              (token t)
                              (history-tokens list))
  (* (class-interpolated-probability model token history-tokens)
     (class-token->token-probability model token)))

;;;

(defun sentence-log-probability (model sentence &key BOS EOS)
  (let ((n (model-n model)))
    (let ((bos-tokens (make-list (1- n) :initial-element BOS))
          (eos-tokens (list EOS))
          (sentence-tokens (sentence-tokens sentence)))
      (let ((tokens (append bos-tokens sentence-tokens eos-tokens)))
        (loop for curr-index from (1- n) to (1- (length tokens))
              for token = (nth curr-index tokens)
              for history-tokens = (subseq tokens (- curr-index (1- n)) curr-index)
              for p = (model-probability model token history-tokens)
              when (= p 0) return (- #xFFFF)
              sum (log p))))))
