;; class 2-gram model
(defpackage :hachee.kkc.impl.class-ngram
  (:use :cl)
  (:shadow :word)
  (:export :set-ex-dict
           :read-kkc-2gram
           :read-kkc-3gram))
(in-package :hachee.kkc.impl.class-ngram)

(defstruct word
  word-id
  class-id)

(defstruct class-model
  ngram-counts
  word-counts)

(defun count-get (hash &rest items)
  (gethash items hash))

(defun transition-log-probability-2gram (class-model weight-2 weight-1
                                         curr-word prev-word)
  (with-accessors ((ngram-counts class-model-ngram-counts)
                   (word-counts class-model-word-counts)) class-model
    (let ((prev-class-id (word-class-id prev-word))
          (curr-class-id (word-class-id curr-word)))
      (+
       (log ;; P(class_curr | class_prev)
        (+
         (* weight-2
            (/ (or (count-get ngram-counts prev-class-id curr-class-id)
                   0)
               (count-get ngram-counts prev-class-id)))
         (* weight-1
            (/ (count-get ngram-counts curr-class-id)
               (count-get ngram-counts)))))
       (log ;; P(word_curr | class_curr)
        (/ (count-get word-counts (word-word-id curr-word))
           (count-get ngram-counts curr-class-id)))))))

(defun transition-log-probability-3gram (class-model
                                         weight-3 weight-2 weight-1
                                         curr-word prev2-word prev1-word)
  (with-accessors ((ngram-counts class-model-ngram-counts)
                   (word-counts class-model-word-counts)) class-model
    (let ((prev2-class-id (word-class-id prev2-word))
          (prev1-class-id (word-class-id prev1-word))
          (curr-class-id (word-class-id curr-word)))
      (+
       (log ;; P(class_curr | class_prev2, class_prev1)
        (+
         (let ((n (count-get ngram-counts
                             prev2-class-id prev1-class-id curr-class-id))
               (d (count-get ngram-counts
                             prev2-class-id prev1-class-id)))
           (if (and n d)
               (* weight-3 (/ n d)) 0))
         (let ((n (count-get ngram-counts prev1-class-id curr-class-id))
               (d (count-get ngram-counts prev1-class-id)))
           (if n
               (* weight-2 (/ n d)) 0))
         (* weight-1
            (/ (count-get ngram-counts curr-class-id)
               (count-get ngram-counts)))))
       (log ;; P(word_curr | class_curr)
        (/ (count-get word-counts (word-word-id curr-word))
           (count-get ngram-counts curr-class-id)))))))

(defstruct unk-model
  ngram-counts
  char-set-size)

(defstruct unk-vocab
  str-id-hash
  BOS-id
  EOS-id
  UNK-id)


(defun unk-model-empty-p (unk-model)
  (with-accessors ((ngram-counts unk-model-ngram-counts)) unk-model
    (= (count-get ngram-counts) 0)))

(defun unk-model-char-log-probability (unk-model)
  (log (/ 1 (unk-model-char-set-size unk-model))))

(defun unk-model-transition-log-probability (unk-model curr-id prev-id)
  (with-accessors ((ngram-counts unk-model-ngram-counts)) unk-model
    (let ((prob
           (+ (* 0.8
                 (/ (or (count-get ngram-counts prev-id curr-id)
                        0)
                    (count-get ngram-counts prev-id)))
              (* 0.2
                 (/ (count-get ngram-counts curr-id)
                    (count-get ngram-counts))))))
      (log prob))))

(defun unk-log-probability (unk-vocab unk-model string)
  (let ((length (length string))
        (unk-char-log-prob (unk-model-char-log-probability unk-model)))
    (if (unk-model-empty-p unk-model)
        (* unk-char-log-prob length)
        (with-accessors ((str-id-hash unk-vocab-str-id-hash)
                         (BOS unk-vocab-BOS-id)
                         (EOS unk-vocab-EOS-id)
                         (UNK unk-vocab-UNK-id)) unk-vocab
          (labels ((rec (prev index sum)
                     (if (= index length)
                         (+ sum (unk-model-transition-log-probability
                                 unk-model EOS prev))
                         (let* ((curr-ch
                                 (aref string index))
                                (curr
                                 (or (gethash (string curr-ch) str-id-hash)
                                     UNK))
                                (log-prob
                                 (+ (unk-model-transition-log-probability
                                     unk-model curr prev)
                                    (if (= curr UNK) unk-char-log-prob 0))))
                           (rec curr (1+ index) (+ sum log-prob))))))
            (rec BOS 0 0))))))

(defstruct class-vocab-entry
  form word)

(defstruct class-vocab
  str-entry-hash
  unk-word
  bos-word
  eos-word)

(defclass kkc (hachee.kkc.convert:convert)
  ((class-vocab
    :initarg :class-vocab
    :reader kkc-class-vocab)
   (class-model
    :initarg :class-model
    :reader kkc-class-model)
   (unk-model
    :initarg :unk-model
    :reader kkc-unk-model)
   (unk-vocab
    :initarg :unk-vocab
    :reader kkc-unk-vocab)
   (ex-dict
    :initarg :ex-dict
    :accessor kkc-ex-dict)))

;;; convert

(defstruct convert-entry
  word form pron origin
  (unk-log-probability 0))

(defun list-convert-entries (class-vocab
                             ex-dict
                             unk-vocab
                             unk-model
                             pron)
  (let ((convert-entry-list nil))
    (let ((str-entry-hash (class-vocab-str-entry-hash class-vocab)))
      (dolist (class-vocab-entry (gethash pron str-entry-hash))
        (push (make-convert-entry
               :word (class-vocab-entry-word class-vocab-entry)
               :form (class-vocab-entry-form class-vocab-entry)
               :pron pron
               :origin :vocab
               :unk-log-probability 0)
              convert-entry-list)))
    (let ((unk-word (class-vocab-unk-word class-vocab)))
      (dolist (ex-dict-entry
               (hachee.kkc.impl.class-ngram.ex-dict:list-entries
                ex-dict pron))
        (push (make-convert-entry
               :word unk-word
               :form (hachee.kkc.impl.class-ngram.ex-dict:entry-form
                      ex-dict-entry)
               :pron pron
               :origin :ex-dict
               :unk-log-probability (hachee.kkc.impl.class-ngram.ex-dict:entry-unk-log-probability ex-dict-entry))
              convert-entry-list))
      (when (< (length pron) 8) ;; Length up to 8
        (push (make-convert-entry
               :word unk-word
               :form (hachee.ja:hiragana->katakana pron)
               :pron pron
               :origin :unk
               :unk-log-probability (unk-log-probability
                                     unk-vocab unk-model pron))
              convert-entry-list)))
    convert-entry-list))

(defmethod hachee.kkc.convert:entry-form ((e convert-entry))
  (convert-entry-form e))

(defmethod hachee.kkc.convert:entry-pron ((e convert-entry))
  (convert-entry-pron e))

(defmethod hachee.kkc.convert:entry-origin ((e convert-entry))
  (convert-entry-origin e))


(defmethod hachee.kkc.convert:convert-begin-entry ((kkc kkc))
  (make-convert-entry :word (class-vocab-bos-word (kkc-class-vocab kkc))))

(defmethod hachee.kkc.convert:convert-end-entry ((kkc kkc))
  (make-convert-entry :word (class-vocab-eos-word (kkc-class-vocab kkc))))

(defmethod hachee.kkc.convert:convert-list-entries-fn ((kkc kkc))
  (let ((class-vocab (kkc-class-vocab kkc))
        (ex-dict (kkc-ex-dict kkc))
        (unk-vocab (kkc-unk-vocab kkc))
        (unk-model (kkc-unk-model kkc)))
    (lambda (pron)
      (list-convert-entries
       class-vocab ex-dict unk-vocab unk-model pron))))


(defclass kkc-2gram (kkc hachee.kkc.convert:2gram-convert)
  ((w2
    :initarg :w2
    :reader kkc-2gram-w2)
   (w1
    :initarg :w1
    :reader kkc-2gram-w1)))

(defmethod hachee.kkc.convert:convert-score-fn ((kkc kkc-2gram))
  (let ((class-model (kkc-class-model kkc))
        (w2 (kkc-2gram-w2 kkc))
        (w1 (kkc-2gram-w1 kkc)))
    (lambda (curr-entry prev-entry)
      (+ (transition-log-probability-2gram class-model w2 w1
                                           (convert-entry-word curr-entry)
                                           (convert-entry-word prev-entry))
         (convert-entry-unk-log-probability curr-entry)))))

(defclass kkc-3gram (kkc hachee.kkc.convert:3gram-convert)
  ((w3
    :initarg :w3
    :reader kkc-3gram-w3)
   (w2
    :initarg :w2
    :reader kkc-3gram-w2)
   (w1
    :initarg :w1
    :reader kkc-3gram-w1)))

(defmethod hachee.kkc.convert:convert-score-fn ((kkc kkc-3gram))
  (let ((class-model (kkc-class-model kkc))
        (w3 (kkc-3gram-w3 kkc))
        (w2 (kkc-3gram-w2 kkc))
        (w1 (kkc-3gram-w1 kkc)))
    (lambda (curr-entry prev2-entry prev1-entry)
      (+ (transition-log-probability-3gram class-model w3 w2 w1
                                           (convert-entry-word curr-entry)
                                           (convert-entry-word prev2-entry)
                                           (convert-entry-word prev1-entry))
         (convert-entry-unk-log-probability curr-entry)))))

;;; lookup

(defstruct lookup-item form origin)

(defmethod hachee.kkc.lookup:item-form ((item lookup-item))
  (lookup-item-form item))

(defmethod hachee.kkc.lookup:item-origin ((item lookup-item))
  (lookup-item-origin item))

(defun list-lookup-items (class-vocab ex-dict pron)
  (let ((items nil))
    (let ((str-entry-hash (class-vocab-str-entry-hash class-vocab)))
      (dolist (class-vocab-entry (gethash pron str-entry-hash))
        (push (make-lookup-item
               :form (class-vocab-entry-form class-vocab-entry)
               :origin :vocab)
              items)))
    (dolist (ex-dict-entry
             (hachee.kkc.impl.class-ngram.ex-dict:list-entries
              ex-dict pron))
      (push (make-lookup-item
             :form (hachee.kkc.impl.class-ngram.ex-dict:entry-form
                    ex-dict-entry)
             :origin :ex-dict)
            items))
    items))

(defmethod hachee.kkc.lookup:execute ((kkc kkc) (pron string)
                                      &key prev next)
  (declare (ignore prev next))
  (list-lookup-items (kkc-class-vocab kkc)
                     (kkc-ex-dict kkc)
                     pron))

;;; ex-dict

(defmethod hachee.kkc.impl.class-ngram.ex-dict-builder:kkc-probability
    ((kkc kkc) (string string))
  (let ((unk-vocab (kkc-unk-vocab kkc))
        (unk-model (kkc-unk-model kkc)))
    (exp (unk-log-probability unk-vocab unk-model string))))

(defmethod hachee.kkc.impl.class-ngram.ex-dict-builder:kkc-contains-p
    ((kkc kkc) item)
  (let ((str-entry-hash (class-vocab-str-entry-hash (kkc-class-vocab kkc)))
        (form (hachee.kkc.impl.class-ngram.ex-dict-builder:item-form item))
        (pron (hachee.kkc.impl.class-ngram.ex-dict-builder:item-pron item)))
    (find form (gethash pron str-entry-hash)
          :test #'string=
          :key #'class-vocab-entry-form)))

(defmethod hachee.kkc.impl.class-ngram.ex-dict-builder:kkc-vocabulary-probability ((kkc kkc))
  (let ((sum-prob 0)
        (str-entry-hash (class-vocab-str-entry-hash (kkc-class-vocab kkc)))
        (unk-vocab (kkc-unk-vocab kkc))
        (unk-model (kkc-unk-model kkc)))
    (loop for entries being the hash-value of str-entry-hash do
        (dolist (entry entries)
          (let* ((form (class-vocab-entry-form entry))
                 (prob (exp (unk-log-probability unk-vocab unk-model form))))
            (incf sum-prob prob))))
    sum-prob))

(defun set-ex-dict (kkc ex-dict-source)
  (let ((ex-dict (hachee.kkc.impl.class-ngram.ex-dict-builder:build
                  kkc ex-dict-source)))
    (setf (kkc-ex-dict kkc) ex-dict)))

;;;

(defvar +UNK+ "<UNK>")
(defvar +BOS+ "<BOS>")
(defvar +EOS+ "<EOS>")

(defun read-class-vocab (vocab-path)
  (let ((hash (make-hash-table :test #'equal))
        (unk-word nil)
        (bos-word nil)
        (eos-word nil))
    (with-open-file (in vocab-path
                        :direction :input
                        :external-format :utf-8)
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (form-pron word-id-str class-id-str)
            (cl-ppcre:split "\\t" line)
          (let ((word (make-word
                       :word-id (parse-integer word-id-str)
                       :class-id (parse-integer class-id-str))))
            (cond ((string= form-pron +UNK+) (setq unk-word word))
                  ((string= form-pron +BOS+) (setq bos-word word))
                  ((string= form-pron +EOS+) (setq eos-word word))
                  (t
                   (destructuring-bind (form pron)
                       (cl-ppcre:split "/" form-pron)
                     (push (make-class-vocab-entry :form form :word word)
                           (gethash pron hash)))))))))
    (assert unk-word)
    (assert bos-word)
    (assert eos-word)
    (make-class-vocab :unk-word unk-word
                      :bos-word bos-word
                      :eos-word eos-word
                      :str-entry-hash hash)))

(defun read-weights (class-weights-path)
  (with-open-file (in class-weights-path
                      :direction :input
                      :external-format :utf-8)
    (let ((line (read-line in nil nil)))
      (let ((items (cl-ppcre:split "\\t" line)))
        (mapcar #'read-from-string items)))))

(defun read-counts (counts-path)
  (let ((counts (make-hash-table :test #'equal)))
    (with-open-file (in counts-path
                        :direction :input
                        :external-format :utf-8)
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (count-str &rest str-list)
            (cl-ppcre:split "," line)
          (let ((count (parse-integer count-str))
                (ngram (mapcar #'parse-integer str-list)))
            (setf (gethash ngram counts) count)))))
    counts))

(defun read-class-model (class-ngram-counts-path
                         class-word-counts-path)
  (let ((ngram-counts (read-counts class-ngram-counts-path))
        (word-counts (read-counts class-word-counts-path)))
    (make-class-model :ngram-counts ngram-counts
                      :word-counts word-counts)))

(defun read-unk-model (unk-ngram-counts-path char-set-size-path)
  (make-unk-model
   :ngram-counts (read-counts unk-ngram-counts-path)
   :char-set-size (with-open-file (in char-set-size-path
                                      :direction :input
                                      :external-format :utf-8)
                    (read in))))

(defun read-unk-vocab (vocab-path)
  (let ((str-id-hash (make-hash-table :test #'equal))
        (UNK-id nil)
        (BOS-id nil)
        (EOS-id nil))
    (with-open-file (in vocab-path
                        :direction :input
                        :external-format :utf-8)
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (id-str str)
            (cl-ppcre:split "\\t" line)
          (let ((id (parse-integer id-str)))
            (cond ((string= str +UNK+) (setq UNK-id id))
                  ((string= str +BOS+) (setq BOS-id id))
                  ((string= str +EOS+) (setq EOS-id id))
                  (t (setf (gethash str str-id-hash) id)))))))
    (assert UNK-id)
    (assert BOS-id)
    (assert EOS-id)
    (make-unk-vocab :str-id-hash str-id-hash
                    :BOS-id BOS-id
                    :EOS-id EOS-id
                    :UNK-id UNK-id)))

(defun read-kkc (class-vocab-path
                 class-ngram-counts-path
                 class-word-counts-path
                 unk-vocab-path
                 unk-ngram-counts-path
                 char-set-size-pah)
  (make-instance 'kkc
   :class-vocab (read-class-vocab class-vocab-path)
   :class-model (read-class-model class-ngram-counts-path
                                  class-word-counts-path)
   :unk-vocab (read-unk-vocab unk-vocab-path)
   :unk-model (read-unk-model unk-ngram-counts-path
                              char-set-size-pah)
   :ex-dict (hachee.kkc.impl.class-ngram.ex-dict:make-ex-dict
             :hash (make-hash-table :test #'equal))))

(defun read-kkc-2gram (class-weights-path
                       class-vocab-path
                       class-ngram-counts-path
                       class-word-counts-path
                       unk-vocab-path
                       unk-ngram-counts-path
                       char-set-size-pah)
  (destructuring-bind (w2 w1)
      (read-weights class-weights-path)
    (change-class (read-kkc class-vocab-path
                            class-ngram-counts-path
                            class-word-counts-path
                            unk-vocab-path
                            unk-ngram-counts-path
                            char-set-size-pah)
                  'kkc-2gram :w2 w2 :w1 w1)))

(defun read-kkc-3gram (class-weights-path
                       class-vocab-path
                       class-ngram-counts-path
                       class-word-counts-path
                       unk-vocab-path
                       unk-ngram-counts-path
                       char-set-size-pah)
  (destructuring-bind (w3 w2 w1)
      (read-weights class-weights-path)
    (change-class (read-kkc class-vocab-path
                            class-ngram-counts-path
                            class-word-counts-path
                            unk-vocab-path
                            unk-ngram-counts-path
                            char-set-size-pah)
                  'kkc-3gram :w3 w3 :w2 w2 :w1 w1)))
