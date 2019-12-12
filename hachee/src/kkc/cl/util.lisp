(defpackage :hachee.kkc.util
  (:use :cl)
  (:export :word->sentence)
  (:import-from :hachee.language-model.vocabulary
                :to-int-or-unk))
(in-package :hachee.kkc.util)

(defun word->sentence (word unknown-word-char-vocabulary)
  (hachee.language-model:make-sentence
   :tokens (mapcar (lambda (char)
                     (to-int-or-unk unknown-word-char-vocabulary char))
                   (hachee.kkc.word:word->pron-chars word))))
