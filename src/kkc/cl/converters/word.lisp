(defpackage :hachee.kkc.converters.word
  (:use :cl :hachee.kkc.converters.converter)
  (:import-from :alexandria :curry))
(in-package :hachee.kkc.converters.word)

(defstruct converter vocabulary language-model kana-kanji-model)

(defun to-int-or-unk (vocab word-pron)
  (hachee.kkc.vocabulary:to-int-or-unk
   vocab (car (cl-ppcre:split "/" word-pron))))

(defmethod probability ((converter converter)
                        (word-pron string)
                        (history-word-pron-list list))
  (let ((vocab (converter-vocabulary converter)))
    (let ((p (hachee.language-model.n-gram:transition-probability
              (converter-language-model converter)
              (to-int-or-unk vocab word-pron)
              (mapcar (curry #'to-int-or-unk vocab)
                      history-word-pron-list))))
      (if (= p 0)
          -100000
          (let ((kana-kanji-model (converter-kana-kanji-model converter)))
            (destructuring-bind (word pron) (cl-ppcre:split "/" word-pron)
              (let ((word-freq (gethash word kana-kanji-model))
                    (pron-freq (gethash pron kana-kanji-model)))
                (if (and word-freq pron-freq)
                    (+ (log p)
                       (log (/ word-freq pron-freq)))
                    -100000))))))))
