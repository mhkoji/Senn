;; class 2-gram model
(defpackage :hachee.kkc.impl.class-2-gram
  (:use :cl)
  (:shadow :word)
  (:export :set-ex-dict
           :read-kkc))
(in-package :hachee.kkc.impl.class-2-gram)

(defstruct word
  word-id
  class-id)

(defstruct class-model
  ngram-counts
  word-counts
  weight-1
  weight-2)

(defun count-get (hash &rest items)
  (gethash items hash))

(defun word-transition-log-probability (class-model curr-word prev-word)
  (with-accessors ((ngram-counts class-model-ngram-counts)
                   (word-counts class-model-word-counts)
                   (weight-1 class-model-weight-1)
                   (weight-2 class-model-weight-2)) class-model
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

(defstruct unk-model
  ngram-counts
  char-set-size)

(defstruct unk-vocab
  str-id-hash
  BOS-id
  EOS-id
  UNK-id)

(defun unk-char-log-probability (unk-model)
  (log (/ 1 (unk-model-char-set-size unk-model))))

(defun unk-transition-log-probability (unk-model curr-id prev-id)
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
        (unk-char-log-prob (unk-char-log-probability unk-model)))
    (with-accessors ((str-id-hash unk-vocab-str-id-hash)
                     (BOS unk-vocab-BOS-id)
                     (EOS unk-vocab-EOS-id)
                     (UNK unk-vocab-UNK-id)) unk-vocab
      (labels ((rec (prev index sum)
                 (if (= index length)
                     (+ sum (unk-transition-log-probability
                             unk-model EOS prev))
                     (let* ((curr-ch
                             (aref string index))
                            (curr
                             (or (gethash (string curr-ch) str-id-hash)
                                 UNK))
                            (log-prob
                             (+ (unk-transition-log-probability
                                 unk-model curr prev)
                                (if (= curr UNK) unk-char-log-prob 0))))
                       (rec curr (1+ index) (+ sum log-prob))))))
        (rec BOS 0 0)))))

(defstruct class-vocab-entry
  form word)

(defstruct class-vocab
  str-entry-hash
  unk-word
  bos-word
  eos-word)

(defstruct kkc
  class-vocab
  class-model
  unk-model
  unk-vocab
  ex-dict)


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
               (hachee.kkc.impl.class-2-gram.ex-dict:list-entries
                ex-dict pron))
        (push (make-convert-entry
               :word unk-word
               :form (hachee.kkc.impl.class-2-gram.ex-dict:entry-form
                      ex-dict-entry)
               :pron pron
               :origin :ex-dict
               :unk-log-probability (hachee.kkc.impl.class-2-gram.ex-dict:entry-unk-log-probability ex-dict-entry))
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

(defmethod hachee.kkc.convert:convert-score-fn ((kkc kkc))
  (let ((class-model (kkc-class-model kkc)))
    (lambda (curr-entry prev-entry)
      (+ (word-transition-log-probability class-model
                                          (convert-entry-word curr-entry)
                                          (convert-entry-word prev-entry))
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
             (hachee.kkc.impl.class-2-gram.ex-dict:list-entries
              ex-dict pron))
      (push (make-lookup-item
             :form (hachee.kkc.impl.class-2-gram.ex-dict:entry-form
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

(defmethod hachee.kkc.impl.class-2-gram.ex-dict-builder:kkc-probability
    ((kkc kkc) (string string))
  (let ((unk-vocab (kkc-unk-vocab kkc))
        (unk-model (kkc-unk-model kkc)))
    (exp (unk-log-probability unk-vocab unk-model string))))

(defmethod hachee.kkc.impl.class-2-gram.ex-dict-builder:kkc-contains-p
    ((kkc kkc) item)
  (let ((str-entry-hash (class-vocab-str-entry-hash (kkc-class-vocab kkc)))
        (form (hachee.kkc.impl.class-2-gram.ex-dict-builder:item-form item))
        (pron (hachee.kkc.impl.class-2-gram.ex-dict-builder:item-pron item)))
    (find form (gethash pron str-entry-hash)
          :test #'string=
          :key #'class-vocab-entry-form)))

(defmethod hachee.kkc.impl.class-2-gram.ex-dict-builder:kkc-vocabulary-probability ((kkc kkc))
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
  (let ((ex-dict (hachee.kkc.impl.class-2-gram.ex-dict-builder:build
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
    (with-open-file (in vocab-path)
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

(defun read-counts (counts-path)
  (let ((counts (make-hash-table :test #'equal)))
    (with-open-file (in counts-path :direction :input)
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (count-str &rest str-list)
            (cl-ppcre:split "," line)
          (let ((count (parse-integer count-str))
                (ngram (mapcar #'parse-integer str-list)))
            (setf (gethash ngram counts) count)))))
    counts))

(defun read-class-model (class-weights-path
                         class-ngram-counts-path
                         class-word-counts-path)
  (let ((ngram-counts (read-counts class-ngram-counts-path))
        (word-counts (read-counts class-word-counts-path))
        (weights (with-open-file (in class-weights-path)
                   (let ((line (read-line in nil nil)))
                     (destructuring-bind (w1 w2)
                         (cl-ppcre:split "\\t" line)
                       (list (read-from-string w1)
                             (read-from-string w2)))))))
    (make-class-model :ngram-counts ngram-counts
                      :word-counts word-counts
                      :weight-1 (nth 0 weights)
                      :weight-2 (nth 1 weights))))

(defun read-unk-model (unk-ngram-counts-path char-set-size-path)
  (make-unk-model
   :ngram-counts (read-counts unk-ngram-counts-path)
   :char-set-size (with-open-file (in char-set-size-path :direction :input)
                    (read in))))

(defun read-unk-vocab (vocab-path)
  (let ((str-id-hash (make-hash-table :test #'equal))
        (UNK-id nil)
        (BOS-id nil)
        (EOS-id nil))
    (with-open-file (in vocab-path)
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
                 class-weights-path
                 class-ngram-counts-path
                 class-word-counts-path
                 unk-vocab-path
                 unk-ngram-counts-path
                 char-set-size-pah)
  (make-kkc
   :class-vocab (read-class-vocab class-vocab-path)
   :class-model (read-class-model class-weights-path
                                  class-ngram-counts-path
                                  class-word-counts-path)
   :unk-vocab (read-unk-vocab unk-vocab-path)
   :unk-model (read-unk-model unk-ngram-counts-path
                              char-set-size-pah)
   :ex-dict (hachee.kkc.impl.class-2-gram.ex-dict:make-ex-dict
             :hash (make-hash-table :test #'equal))))
