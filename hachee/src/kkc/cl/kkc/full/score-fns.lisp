(defpackage :hachee.kkc.full.score-fns
  (:use :cl)
  (:import-from :hachee.kkc.entry
                :entry-word
                :entry-token
                :entry-origin)
  (:import-from :hachee.language-model.n-gram
                :transition-probability
                :sentence-log-probability)
  (:import-from :hachee.language-model.vocabulary
                :to-int
                :to-int-or-unk
                :to-int-or-nil
                :+UNK+ :+BOS+ :+EOS+)
  (:export :unknown-word-log-probability
           :get-for-conv
           :get-for-lookup))
(in-package :hachee.kkc.full.score-fns)

(defun from-vocabulary-p (entry)
  (eql (entry-origin entry) :vocabulary))

(defun from-extended-dictionary-p (entry)
  (eql (entry-origin entry) :extended-dictionary))

(defun unknown-word-log-probability
    (entry
     unknown-word-char-vocabulary
     unknown-word-char-n-gram-model
     probability-for-extended-dictionary-words)
  (let ((char-bos-token (to-int unknown-word-char-vocabulary +BOS+))
        (char-eos-token (to-int unknown-word-char-vocabulary +EOS+)))
    (let* ((char-sentence
            (hachee.kkc.util:word->sentence (entry-word entry)
                                            unknown-word-char-vocabulary))
           (log-prob-by-unknown-word-n-gram
            (sentence-log-probability unknown-word-char-n-gram-model
                                      char-sentence
                                      :BOS char-bos-token
                                      :EOS char-eos-token)))
      (if (from-extended-dictionary-p entry)
          (log (+ (exp log-prob-by-unknown-word-n-gram)
                  probability-for-extended-dictionary-words))
          log-prob-by-unknown-word-n-gram))))

(defun get-for-conv (&key word-n-gram-model
                          unknown-word-log-probability-fn)
  (lambda (curr-entry prev-entry)
    (let ((p (transition-probability word-n-gram-model
                                     (entry-token curr-entry)
                                     (list (entry-token prev-entry)))))
      (if (= p 0)
          -10000
          (+ (log p)
             (if (from-vocabulary-p curr-entry)
                 0
                 (funcall unknown-word-log-probability-fn curr-entry)))))))

(defun get-for-lookup (prev-word next-word
                       &key word-vocabulary
                            word-n-gram-model
                            unknown-word-char-vocabulary
                            unknown-word-char-n-gram-model
                            (probability-for-extended-dictionary-words 0))
  (declare (ignore probability-for-extended-dictionary-words))
  (let ((word-unk-token (to-int word-vocabulary +UNK+))
        (char-bos-token (to-int unknown-word-char-vocabulary +BOS+))
        (char-eos-token (to-int unknown-word-char-vocabulary +EOS+))
        (prev-token (to-int-or-unk word-vocabulary prev-word))
        (fail-safe-score -10000))
    (labels ((transit-score (prev-token curr-word)
               (let ((curr-token (to-int-or-nil word-vocabulary curr-word)))
                 (if curr-token
                     (let ((p (transition-probability
                               word-n-gram-model
                               curr-token (list prev-token))))
                       (if (/= p 0)
                           (log p)
                           fail-safe-score))
                     (let ((p (transition-probability
                               word-n-gram-model
                               word-unk-token (list prev-token))))
                       (if (/= p 0)
                           (+ (log p)
                              (let ((log-prob-by-unknown-word-n-gram
                                     (sentence-log-probability
                                      unknown-word-char-n-gram-model
                                      (hachee.kkc.util:word->sentence
                                       curr-word
                                       unknown-word-char-vocabulary)
                                      :BOS char-bos-token
                                      :EOS char-eos-token)))
                                ;; TODO: support for extended dictionary
                                log-prob-by-unknown-word-n-gram))
                           fail-safe-score))))))
      (let ((score-cache (make-hash-table :test #'equal)))
        (lambda (curr-word)
          (let ((key (hachee.kkc.word:word->key curr-word)))
            (or (gethash key score-cache)
                (setf (gethash key score-cache)
                      (+ (transit-score prev-token
                                        curr-word)
                         (transit-score (to-int-or-unk word-vocabulary
                                                       curr-word)
                                        next-word))))))))))
