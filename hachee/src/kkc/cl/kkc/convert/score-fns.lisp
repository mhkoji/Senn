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
  (:import-from :hachee.language-model.vocabulary
                :to-int
                :to-int-or-unk
                :to-int-or-nil
                :+UNK+ :+BOS+ :+EOS+)
  (:export :of-form-pron-simple
           :of-form-pron))
(in-package :hachee.kkc.convert.score-fns)

(defun node-word-from-extended-dictionary-p (node)
  (eql (node-word-origin node) :extended-dictionary))

(defun of-form-pron-simple (&key word-vocabulary word-n-gram-model)
  (let ((fail-safe-score -10000))
    (lambda (curr-node prev-node)
      (let ((curr-token (to-int-or-nil word-vocabulary
                                       (node-word curr-node)))
            (prev-token (to-int-or-unk word-vocabulary
                                       (node-word prev-node))))
        (if curr-token
            (let ((p (transition-probability
                      word-n-gram-model curr-token (list prev-token))))
              (if (/= p 0)
                  (log p)
                  fail-safe-score))
            fail-safe-score)))))

(defun of-form-pron (&key word-vocabulary
                          word-n-gram-model
                          unknown-word-char-vocabulary
                          unknown-word-char-n-gram-model
                          (probability-for-extended-dictionary-words 0))
  (let ((word-unk-token (to-int word-vocabulary +UNK+))
        (char-bos-token (to-int unknown-word-char-vocabulary +BOS+))
        (char-eos-token (to-int unknown-word-char-vocabulary +EOS+))
        (fail-safe-score -10000))
    (lambda (curr-node prev-node)
      (let ((curr-token (to-int-or-nil word-vocabulary
                                       (node-word curr-node)))
            (prev-token (to-int-or-unk word-vocabulary
                                       (node-word prev-node))))
        (if curr-token
            (let ((p (transition-probability
                      word-n-gram-model curr-token (list prev-token))))
              (if (/= p 0)
                  (log p)
                  fail-safe-score))
            (let ((p (transition-probability
                      word-n-gram-model word-unk-token (list prev-token))))
              (if (/= p 0)
                  (+ (log p)
                     (let ((log-prob-by-unknown-word-n-gram
                            (sentence-log-probability
                             unknown-word-char-n-gram-model
                             (hachee.kkc.util:word->sentence
                              (node-word curr-node)
                              unknown-word-char-vocabulary)
                             :BOS char-bos-token
                             :EOS char-eos-token)))
                       (if (node-word-from-extended-dictionary-p curr-node)
                           (log
                            (+ (exp log-prob-by-unknown-word-n-gram)
                               probability-for-extended-dictionary-words))
                           log-prob-by-unknown-word-n-gram)))
                  fail-safe-score)))))))
