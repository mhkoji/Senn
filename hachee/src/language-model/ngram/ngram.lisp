(defpackage :hachee.language-model.ngram
  (:use :cl)
  (:export :sentence
           :sentence-tokens
           :make-sentence
           :train
           :get-count
           :transition-probability
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

(defstruct count-spec n BOS EOS)

(defun add-ngram-counts (freq tokens spec)
  (hachee.language-model.ngram.probability:add-ngram-counts
   freq tokens
   (count-spec-BOS spec)
   (count-spec-EOS spec)
   (count-spec-n spec)))

;;;

(defstruct sentence tokens)

(defgeneric train (model sentence-provider &key BOS EOS))

(defmethod train ((model t) (list list) &key BOS EOS)
  (labels ((next ()
             (pop list)))
    (train model #'next :BOS BOS :EOS EOS)))

(defgeneric transition-probability (model token history-tokens))

;; ngram model is implemented as a little application of freq.
;; An n-gram language model provides the functions of:
;; - counting the ngram tokens in a sentence
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

(defmacro do-sentence ((s sentence-provider) &body body)
  `(loop for ,s = (funcall ,sentence-provider)
         while ,s do (progn ,@body)))


(defmethod train ((model model) (sentence-provider function) &key BOS EOS)
  (let ((freq (model-freq model))
        (spec (make-count-spec :n (model-n model)
                               :BOS BOS
                               :EOS EOS)))
    (do-sentence (sentence sentence-provider)
      (add-ngram-counts freq (sentence-tokens sentence) spec)))
  model)

(defmethod transition-probability ((model model)
                                   (token t)
                                   (history-tokens list))
  (hachee.language-model.ngram.probability:interpolated-probability
   (model-freq model)
   (model-weights model)
   token
   history-tokens))

;;;

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
         (class-EOS (class-token classifier EOS))
         (model-freq (model-freq model))
         (count-spec (make-count-spec
                      :n (model-n model)
                      :BOS (class-token classifier BOS)
                      :EOS class-EOS)))
    (do-sentence (sentence sentence-provider)
      (let ((sentence-tokens (sentence-tokens sentence)))
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
          (add-ngram-counts model-freq sentence-class-tokens count-spec)))))
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
      (hachee.language-model.ngram.probability:interpolated-probability
       (model-freq class-model)
       (model-weights class-model)
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
          (sentence-tokens (sentence-tokens sentence)))
      (let ((tokens (append bos-tokens sentence-tokens eos-tokens)))
        (loop for curr-index from (1- n) to (1- (length tokens))
              for p = (transition-probability
                       model
                       (nth curr-index tokens)
                       (subseq tokens (- curr-index (1- n)) curr-index))
              when (= p 0) return (- #xFFFF)
              sum (log p))))))
