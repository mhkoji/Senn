(defpackage :hachee.kkc.convert.cost-fns
  (:use :cl)
  (:import-from :alexandria
                :curry)
  (:import-from :hachee.kkc.word.vocabulary
                :to-int :+UNK+)
  (:export :of-word-pron :of-word))
(in-package :hachee.kkc.convert.cost-fns)

(defun of-word-pron (&key vocabulary language-model)
  (let ((to-int
         (curry #'hachee.kkc.word.vocabulary:to-int vocabulary))
        (to-int-or-unk
         (curry #'hachee.kkc.word.vocabulary:to-int-or-unk vocabulary)))
    (lambda (word history-word-list)
      (let ((token-or-nil (funcall to-int word))
            (history-tokens (mapcar to-int-or-unk history-word-list)))
        (let ((p (hachee.language-model.n-gram:transition-probability
                  language-model
                  (or token-or-nil (funcall to-int +UNK+))
                  history-tokens)))
          (if token-or-nil
              (log p)
              (+ (if (= p 0) -10000 (log p))
                 (log (hachee.kkc.models.unknown-word:probability
                       nil word)))))))))


(defun of-word (&key vocabulary language-model kana-kanji-model)
  (let ((to-int-or-unk
         (curry #'hachee.kkc.word.vocabulary:to-int-or-unk vocabulary)))
    (lambda (word history-word-list)
      (let ((p (hachee.language-model.n-gram:transition-probability
                language-model
                (funcall to-int-or-unk word)
                (mapcar to-int-or-unk history-word-list))))
        (if (= p 0)
            -100000
            (let ((form-freq (gethash (hachee.kkc.word:word-form word)
                                      kana-kanji-model))
                  (pron-freq (gethash (hachee.kkc.word:word-pron word)
                                      kana-kanji-model)))
              (if (and form-freq pron-freq)
                  (+ (log p)
                     (log (/ form-freq pron-freq)))
                  -100000)))))))
