(defpackage :hachee.kkc.convert.score-fns
  (:use :cl)
  (:import-from :alexandria
                :if-let)
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
    (lambda (curr-word prev-words)
      (let ((prev-tokens (mapcar (lambda (w)
                                   (to-int-or-unk vocabulary w))
                                 prev-words)))
        (if-let ((curr-token (to-int-or-nil vocabulary curr-word)))
          (let ((p (hachee.language-model.n-gram:transition-probability
                    n-gram-model
                    curr-token
                    prev-tokens)))
            (if (/= p 0)
                (log p)
                fail-safe-score))
          fail-safe-score)))))

(defun of-form-pron-unk-supported (&key vocabulary
                                        n-gram-model
                                        unknown-word-vocabulary
                                        unknown-word-n-gram-model)
  (let ((unk-token (to-int vocabulary +UNK+))
        (fail-safe-score -10000))
    (lambda (curr-word prev-words)
      (let ((prev-tokens (mapcar (lambda (w)
                                   (to-int-or-unk vocabulary w))
                                 prev-words)))
        (if-let ((curr-token (to-int-or-nil vocabulary curr-word)))
          (let ((p (hachee.language-model.n-gram:transition-probability
                    n-gram-model
                    curr-token
                    prev-tokens)))
            (if (/= p 0)
                (log p)
                fail-safe-score))
          (let ((p (hachee.language-model.n-gram:transition-probability
                    n-gram-model
                    unk-token
                    prev-tokens)))
            (if (/= p 0)
                (let ((unknown-word-prediction-log-p
                       (hachee.language-model.n-gram:sentence-log-probability
                        unknown-word-n-gram-model
                        (hachee.language-model:make-sentence
                         :tokens (word->char-tokens
                                  curr-word
                                  unknown-word-vocabulary))
                        :BOS (to-int unknown-word-vocabulary +BOS+)
                        :EOS (to-int unknown-word-vocabulary +EOS+))))
                  (+ (log p) unknown-word-prediction-log-p))
                fail-safe-score)))))))
