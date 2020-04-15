(defpackage :hachee.kkc.util
  (:use :cl)
  (:export :unit->sentence)
  (:import-from :hachee.language-model.vocabulary
                :to-int-or-unk))
(in-package :hachee.kkc.util)

(defun unit->sentence (unit unknown-word-char-vocabulary)
  (hachee.language-model:make-sentence
   :tokens (mapcar (lambda (pron-unit)
                     (to-int-or-unk
                      unknown-word-char-vocabulary
                      (hachee.kkc.dictionary:unit->key pron-unit)))
                   (hachee.kkc.dictionary:unit->pron-units unit))))
