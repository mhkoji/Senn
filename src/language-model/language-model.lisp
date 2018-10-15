(defpackage :hachee.language-model
  (:use :cl)
  (:import-from :alexandria
                :curry)
  (:export :sentence
           :sentence-tokens
           :make-sentence
           :train
           :transition-probability
           :model
           :make-model
           :class-model
           :make-class-model))
(in-package :hachee.language-model)

(defmacro inchash (key hash)
  `(incf (gethash ,key ,hash 0)))

(defstruct sentence tokens)

;; n-gram model
(defclass model ()
  ((n-gram
    :initarg :n-gram
    :reader model-n-gram)))

(defgeneric train (model sentences &key BOS EOS))

(defgeneric transition-probability (model token history-tokens))


(defmethod train ((model model) (sentences list) &key BOS EOS)
  (dolist (sentence sentences)
    (hachee.language-model.n-gram:add-counts
     (model-n-gram model) (sentence-tokens sentence) :BOS BOS :EOS EOS))
  model)

(defmethod transition-probability ((model model)
                                   (token t)
                                   (history-tokens list))
  (hachee.language-model.n-gram:interpolated-probability
   (model-n-gram model) token history-tokens))


(defclass class-model (model)
  ((classifier
    :initarg :classifier
    :reader class-model-classifier)
   (token-freq
    :initform (make-hash-table :test #'equal)
    :reader class-model-token-freq)
   (class-token-freq
    :initform (make-hash-table :test #'equal)
    :reader class-model-class-token-freq)))

(defstruct classifier to-class-map)

(defun class-token (classifier x)
  (gethash x (classifier-to-class-map classifier)))

(defmethod train ((model class-model) (sentences list) &key BOS EOS)
  (let ((n-gram (model-n-gram model))
        (classifier (class-model-classifier model))
        (token-freq (class-model-token-freq model))
        (class-token-freq (class-model-class-token-freq model)))
    (let ((bos-class-token (class-token classifier BOS))
          (eos-class-token (class-token classifier EOS)))
      (dolist (sentence sentences)
        (let* ((sentence-tokens (sentence-tokens sentence))
               (sentence-class-tokens (mapcar
                                       (curry #'class-token classifier)
                                       sentence-tokens)))
          (dolist (token sentence-tokens)
            (inchash token token-freq))
          (dolist (class-token sentence-class-tokens)
            (inchash class-token class-token-freq))
          (hachee.language-model.n-gram:add-counts
           n-gram
           sentence-class-tokens
           :BOS bos-class-token :EOS eos-class-token)))))
  model)

(defun class-interpolated-probability (class-model
                                       token
                                       history-tokens)
  (let ((n-gram (model-n-gram class-model))
        (classifier (class-model-classifier class-model)))
    (let ((class-token
           (class-token classifier token))
          (history-class-tokens
           (mapcar (curry #'class-token classifier) history-tokens)))
      (hachee.language-model.n-gram:interpolated-probability
       n-gram class-token history-class-tokens))))

(defun class-token->token-probability (class-model token)
  (let ((classifier (class-model-classifier class-model))
        (token-freq (class-model-token-freq class-model))
        (class-token-freq (class-model-class-token-freq class-model)))
    (let ((class-token (class-token classifier token)))
      (/ (gethash token token-freq)
         (gethash class-token class-token-freq)))))

(defmethod transition-probability ((model class-model)
                                   (token t)
                                   (history-tokens list))
  (* (class-interpolated-probability model token history-tokens)
     (class-token->token-probability model token)))


(defun sentence-log-probability (model sentence &key BOS EOS)
  (let ((n (hachee.language-model.n-gram:n-gram-n (model-n-gram model))))
    (let ((bos-tokens (make-list (1- n) :initial-element BOS))
          (eos-tokens (list EOS)))
      (let ((tokens (append bos-tokens
                            (sentence-tokens sentence)
                            eos-tokens)))
        (loop for curr-index from n to (1- (length tokens))
              sum (log (transition-probability
                        model
                        (nth curr-index tokens)
                        (subseq tokens (- curr-index n) n))))))))
