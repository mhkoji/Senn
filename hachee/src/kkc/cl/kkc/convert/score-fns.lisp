(defpackage :hachee.kkc.convert.score-fns
  (:use :cl)
  (:import-from :alexandria
                :if-let)
  (:import-from :hachee.kkc.convert
                :node-word
                :node-word-origin)
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

(defun node-word-from-extended-dictionary-p (node)
  (eql (node-word-origin node) :extended-dictionary))

(defun word->char-tokens (word unknown-word-vocabulary)
  (let ((pron (hachee.kkc.word:word-pron word)))
    (loop for char across pron
          collect (to-int-or-unk unknown-word-vocabulary
                                 (hachee.kkc.word:make-word
                                  :form (string char)
                                  :pron (string char))))))

(defun of-form-pron (&key vocabulary n-gram-model)
  (let ((fail-safe-score -10000))
    (lambda (curr-node prev-node)
      (let ((curr-token (to-int-or-nil vocabulary (node-word curr-node)))
            (prev-token (to-int-or-unk vocabulary (node-word prev-node))))
        (if curr-token
            (let ((p (transition-probability
                      n-gram-model curr-token (list prev-token))))
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
    (lambda (curr-node prev-node)
      (let ((curr-token (to-int-or-nil vocabulary (node-word curr-node)))
            (prev-token (to-int-or-unk vocabulary (node-word prev-node))))
        (if curr-token
            (let ((p (transition-probability
                      n-gram-model curr-token (list prev-token))))
              (if (/= p 0)
                  (log p)
                  fail-safe-score))
            (let ((p (transition-probability
                      n-gram-model unk-token (list prev-token))))
              (if (/= p 0)
                  (let ((log-probability-by-unknown-word-n-gram
                         (sentence-log-probability
                          unknown-word-n-gram-model
                          (hachee.language-model:make-sentence
                           :tokens (word->char-tokens
                                    (node-word curr-node)
                                    unknown-word-vocabulary))
                          :BOS (to-int unknown-word-vocabulary +BOS+)
                          :EOS (to-int unknown-word-vocabulary +EOS+))))
                    (+ (log p)
                       (if (node-word-from-extended-dictionary-p curr-node)
                           (log
                            (+ (exp log-probability-by-unknown-word-n-gram)
                               probability-for-extended-dictionary-words))
                           log-probability-by-unknown-word-n-gram)))
                  fail-safe-score)))))))
