(defpackage :hachee.kkc.converters.word-pron
  (:use :cl :hachee.kkc.converters.converter)
  (:import-from :alexandria
                :curry)
  (:import-from :hachee.kkc.vocabulary
                :to-int :to-int-or-unk)
  (:export :make-converter))
(in-package :hachee.kkc.converters.word-pron)

(defstruct converter vocabulary language-model)

(defmethod probability ((converter converter)
                        (word-pron string)
                        (history-word-pron-list list))
  (let ((vocab (converter-vocabulary converter))
        (model (converter-language-model converter)))
    (let ((token-or-nil (to-int vocab word-pron))
          (history-tokens (mapcar (curry #'to-int-or-unk vocab)
                                  history-word-pron-list)))
      (let ((p (hachee.language-model.n-gram:transition-probability
                model
                (or token-or-nil
                    (to-int vocab hachee.kkc.vocabulary:+UNK+))
                history-tokens)))
        (if token-or-nil
            (log p)
            (+ (if (= p 0) -10000 (log p))
               (log (hachee.kkc.models.unknown-word:probability
                     nil
                     word-pron))))))))
