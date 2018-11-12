(defpackage :hachee.kkc.convert.cost-fns
  (:use :cl)
  (:import-from :alexandria
                :curry)
  (:import-from :hachee.kkc.vocabulary
                :to-int)
  (:export :of-word-pron :of-word))
(in-package :hachee.kkc.convert.cost-fns)

(defun of-word-pron (&key vocabulary language-model)
  (let ((to-int-or-unk #'hachee.kkc.vocabulary:to-int-or-unk))
    (lambda (word-pron history-word-pron-list)
      (let ((token-or-nil (to-int vocabulary word-pron))
            (history-tokens (mapcar (curry to-int-or-unk vocabulary)
                                    history-word-pron-list)))
        (let ((p (hachee.language-model.n-gram:transition-probability
                  language-model
                  (or token-or-nil
                      (to-int vocabulary hachee.kkc.vocabulary:+UNK+))
                  history-tokens)))
          (if token-or-nil
              (log p)
              (+ (if (= p 0) -10000 (log p))
                 (log (hachee.kkc.models.unknown-word:probability
                       nil
                       word-pron)))))))))


(defun of-word (&key vocabulary language-model kana-kanji-model)
  (labels ((to-int-or-unk (word-pron)
             (hachee.kkc.vocabulary:to-int-or-unk
              vocabulary (car (cl-ppcre:split "/" word-pron)))))
    (lambda (word-pron history-word-pron-list)
        (let ((p (hachee.language-model.n-gram:transition-probability
                  language-model
                  (to-int-or-unk word-pron)
                  (mapcar #'to-int-or-unk history-word-pron-list))))
          (if (= p 0)
              -100000
              (destructuring-bind (word pron) (cl-ppcre:split "/" word-pron)
                (let ((word-freq (gethash word kana-kanji-model))
                      (pron-freq (gethash pron kana-kanji-model)))
                  (if (and word-freq pron-freq)
                      (+ (log p)
                         (log (/ word-freq pron-freq)))
                      -100000))))))))
