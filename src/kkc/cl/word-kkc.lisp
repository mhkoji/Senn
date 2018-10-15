(defpackage :hachee.kkc.word
  (:use :cl :hachee.kkc)
  (:import-from :alexandria :curry))
(in-package :hachee.kkc.word)

(defclass word-kkc (kkc)
  ((language-model
    :initform nil
    :initarg :language-model
    :reader word-kkc-language-model)
   (kana-kanji-model
    :initform nil
    :initarg :kana-kanji-model
    :reader word-kkc-kana-kanji-model)))

(defun to-int-or-unk (vocab word-pron)
  (hachee.kkc.vocabulary:to-int-or-unk
   vocab (car (cl-ppcre:split "/" word-pron))))

(defmethod transition-log-probability ((kkc word-kkc)
                                       (word-pron string)
                                       (history-word-pron-list list))
  (let ((vocab (kkc-vocabulary kkc)))
    (let ((p (hachee.language-model:transition-probability
              (word-kkc-language-model kkc)
              (to-int-or-unk vocab word-pron)
              (mapcar (curry #'to-int-or-unk vocab)
                      history-word-pron-list))))
      (if (= p 0)
          -100000
          (let ((kana-kanji-model (word-kkc-kana-kanji-model kkc)))
            (destructuring-bind (word pron)
                (cl-ppcre:split "/" word-pron)
              (let ((word-freq (gethash word kana-kanji-model))
                    (pron-freq (gethash pron kana-kanji-model)))
                (if (and word-freq pron-freq)
                    (+ (log p)
                       (log (/ word-freq pron-freq)))
                    -100000))))))))
