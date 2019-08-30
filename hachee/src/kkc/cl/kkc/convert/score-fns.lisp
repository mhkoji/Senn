(defpackage :hachee.kkc.convert.score-fns
  (:use :cl)
  (:import-from :alexandria
                :if-let)
  (:import-from :hachee.language-model.n-gram
                :transition-probability
                :sentence-log-probability)
  (:import-from :hachee.kkc.word.vocabulary
                :to-int
                :to-int-or-unk
                :to-int-or-nil
                :+UNK+ :+BOS+ :+EOS+)
  (:export :of-form-pron
           :of-form-pron-unk-supported))
(in-package :hachee.kkc.convert.score-fns)

(defun word->char-tokens (word unknown-word-vocabulary)
  (let ((pron (hachee.kkc.word:word-pron word)))
    (loop for char across pron
          collect (to-int-or-unk unknown-word-vocabulary
                                 (hachee.kkc.word:make-word
                                  :form (string char)
                                  :pron (string char))))))

(defun of-form-pron (&key vocabulary n-gram-model)
  (let ((fail-safe-score -10000))
    (lambda (curr-word curr-word-from-extended-dictionary-p prev-words)
      (declare (ignore curr-word-from-extended-dictionary-p))
      (let ((curr-token
             (to-int-or-nil vocabulary curr-word))
            (prev-tokens
             (mapcar (lambda (w) (to-int-or-unk vocabulary w))
                     prev-words)))
        (if curr-token
            (let ((p (transition-probability
                      n-gram-model curr-token prev-tokens)))
              (if (/= p 0)
                  (log p)
                  fail-safe-score))
            fail-safe-score)))))

(defun of-form-pron-unk-supported
    (&key vocabulary
          n-gram-model
          unknown-word-vocabulary
          unknown-word-n-gram-model
          (probability-for-extended-dictionary-words 0))
  (let ((unk-token (to-int vocabulary +UNK+))
        (fail-safe-score -10000))
    (lambda (curr-word curr-word-from-extended-dictionary-p prev-words)
      (let ((curr-token
             (to-int-or-nil vocabulary curr-word))
            (prev-tokens
             (mapcar (lambda (w) (to-int-or-unk vocabulary w))
                     prev-words)))
        (if curr-token
            (let ((p (transition-probability
                      n-gram-model curr-token prev-tokens)))
              (if (/= p 0)
                  (log p)
                  fail-safe-score))
            (let ((p (transition-probability
                      n-gram-model unk-token prev-tokens)))
              (if (/= p 0)
                  (let ((log-probability-by-unknown-word-n-gram
                         (sentence-log-probability
                          unknown-word-n-gram-model
                          (hachee.language-model:make-sentence
                           :tokens (word->char-tokens
                                    curr-word
                                    unknown-word-vocabulary))
                          :BOS (to-int unknown-word-vocabulary +BOS+)
                          :EOS (to-int unknown-word-vocabulary +EOS+))))
                    (+ (log p)
                       (if curr-word-from-extended-dictionary-p
                           (log
                            (+ (exp log-probability-by-unknown-word-n-gram)
                               probability-for-extended-dictionary-words))
                           log-probability-by-unknown-word-n-gram)))
                  fail-safe-score)))))))
