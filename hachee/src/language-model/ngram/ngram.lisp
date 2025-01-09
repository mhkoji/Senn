(defpackage :hachee.language-model.ngram
  (:use :cl)
  (:export :sentence
           :sentence-tokens
           :make-sentence
           :with-model-add-counts
           :do-model-ngram-count
           :model-probability
           :model-weights
           :model
           :class-model
           :do-class-model-token-count
           :make-classifier
           :sentence-log-probability))
(in-package :hachee.language-model.ngram)

(defstruct sentence tokens)

(defgeneric model-probability (model token history-tokens))

(defgeneric model-add-counts-fn (model BOS EOS))

(defmacro with-model-add-counts ((add-counts model &key BOS EOS)
                                 &body body)
  (let ((sym-BOS (gensym))
        (sym-EOS (gensym)))
    `(let ((,sym-BOS ,BOS)
           (,sym-EOS ,EOS))
       (let ((fn (model-add-counts-fn ,model ,sym-BOS ,sym-EOS)))
         (labels ((,add-counts (sentence)
                    (funcall fn sentence)))
           (progn ,@body))))))

;;;

(defclass model-mixin ()
  ((freq
    :initform (hachee.language-model.ngram.freq:make-freq)
    :reader model-freq)
   (weights
    :initform nil
    :initarg :weights
    :reader model-weights)))

(defmethod initialize-instance :after ((model model-mixin) &key)
  (with-slots (weights) model
    (when (not weights)
      (setf weights (list 0.8d0 0.2d0)))))

(defun model-n (model)
  (length (model-weights model)))

(defun model-interpolated-probability (model token history-tokens)
  (reduce #'+ (hachee.language-model.ngram.probability:weighted-list
               (model-freq model)
               (model-weights model)
               token
               history-tokens)))

(defmacro do-model-ngram-count ((tokens count model) &body body)
  `(hachee.language-model.ngram.freq:do-ngram-count
       (,tokens ,count (model-freq ,model))
     ,@body))

;; ngram model is implemented as a little application of freq.
;; An n-gram language model provides the functions of:
;; - counting the ngram tokens in a sentence
;; - computing the probability of an event w_1, ..., w_{n-1} to w_n
(defclass model (model-mixin)
  ())

(defmethod model-add-counts-fn ((model model) BOS EOS)
  (hachee.language-model.ngram.freq:with-add-counts
      (add-ngram-counts (model-freq model)
                        :n (model-n model)
                        :BOS BOS
                        :EOS EOS)
    (lambda (sentence)
      (add-ngram-counts (sentence-tokens sentence)))))

(defmethod model-probability ((model model) (token t) (history-tokens list))
  (model-interpolated-probability model token history-tokens))

;;;


(defstruct classifier to-class-map)

(defmacro inchash (key hash)
  `(incf (gethash ,key ,hash 0)))

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

(defmethod model-add-counts-fn ((model class-model) BOS EOS)
  (let* ((classifier (class-model-classifier model))
         (token-freq (class-model-token-freq model))
         (class-token-freq (class-model-class-token-freq model))
         (class-EOS (class-token classifier EOS)))
    (hachee.language-model.ngram.freq:with-add-counts
        (add-ngram-counts (model-freq model)
                          :n (model-n model)
                          :BOS (class-token classifier BOS)
                          :EOS class-EOS)
      (lambda (sentence)
        (let* ((tokens (sentence-tokens sentence))
               (class-tokens (mapcar (lambda (x)
                                       (class-token classifier x))
                                     tokens)))
          (add-ngram-counts class-tokens)
          (add-token-counts token-freq tokens EOS)
          (add-class-token-counts class-token-freq class-tokens class-EOS))))))

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
      (model-interpolated-probability class-model
                                      class-token
                                      class-history-tokens))))

(defmethod model-probability ((model class-model)
                              (token t)
                              (history-tokens list))
  (* (class-interpolated-probability model token history-tokens)
     (class-token->token-probability model token)))

(defmacro do-class-model-token-count ((token count model) &body body)
  (let ((sym-class-model (gensym)))
    `(let ((,sym-class-model ,model))
       (let* ((token-freq (class-model-token-freq ,sym-class-model))
              (token-list (sort (loop for token being the hash-key of token-freq
                                      collect token)
                                #'<)))
         (dolist (,token token-list)
           (let ((,count (gethash ,token token-freq)))
             (progn ,@body)))))))

;;;

(defun sentence-log-probability (model sentence &key BOS EOS)
  (let ((n (model-n model)))
    (let ((bos-tokens (make-list (1- n) :initial-element BOS))
          (eos-tokens (list EOS))
          (sentence-tokens (sentence-tokens sentence)))
      (let ((tokens (append bos-tokens sentence-tokens eos-tokens)))
        (loop for curr-index from (1- n) to (1- (length tokens))
              for token = (nth curr-index tokens)
              for history-tokens = (subseq tokens
                                           (- curr-index (1- n))
                                           curr-index)
              for p = (model-probability model token history-tokens)
              when (= p 0) return (- #xFFFF)
              sum (log p))))))
