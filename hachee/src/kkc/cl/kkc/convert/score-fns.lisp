(defpackage :hachee.kkc.convert.score-fns
  (:use :cl)
  (:import-from :alexandria
                :if-let :curry)
  (:import-from :hachee.kkc.word.vocabulary
                :to-int-or-unk :to-int :+UNK+)
  (:export :of-form-pron :of-kana-kanji))
(in-package :hachee.kkc.convert.score-fns)

(defun of-form-pron (&key vocabulary language-model)
  (lambda (curr-word prev-words)
    (let ((curr-token-or-nil (to-int vocabulary curr-word))
          (prev-tokens (mapcar (lambda (w)
                                 (to-int-or-unk vocabulary w))
                               prev-words)))
      (let ((p (hachee.language-model.n-gram:transition-probability
                language-model
                (or curr-token-or-nil
                    (to-int-or-unk vocabulary +UNK+))
                prev-tokens)))
        (cond ((and curr-token-or-nil (/= p 0))
               (log p))
              ((/= p 0)
               (+ (log p)
                  -10000
                  #+nil
                  (log (hachee.kkc.models.unknown-word:probability
                        nil
                        curr-word))))
              (t -10000))))))

(defun of-kana-kanji (&key vocabulary language-model kana-kanji-model)
  (lambda (curr-word prev-words)
    (let ((curr-token (to-int-or-unk vocabulary curr-word))
          (prev-tokens (mapcar (lambda (w)
                                 (to-int-or-unk vocabulary w))
                               prev-words)))
      (let ((p (hachee.language-model.n-gram:transition-probability
                language-model curr-token prev-tokens)))
        (if (= p 0)
            -100000
            (let ((form-freq (gethash (hachee.kkc.word:word-form curr-word)
                                      kana-kanji-model))
                  (pron-freq (gethash (hachee.kkc.word:word-pron curr-word)
                                      kana-kanji-model)))
              (if (and form-freq pron-freq)
                  (+ (log p)
                     (log (/ form-freq pron-freq)))
                  -100000)))))))
