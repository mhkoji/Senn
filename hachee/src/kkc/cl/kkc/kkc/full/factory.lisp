(defpackage :hachee.kkc.full.factory
  (:use :cl)
  (:import-from :hachee.kkc.build
                :build-vocabulary-with-unk
                :build-dictionary
                :build-n-gram-model
                :build-unknown-word-vocabulary
                :build-unknown-word-n-gram-model)
  (:export :unk-supported))
(in-package :hachee.kkc.full.factory)

(defun unk-supported (pathnames &key word-dictionary
                                     tankan-dictionary
                                     extended-dictionary)
  (assert (<= 2 (length pathnames)))
  (let* ((vocabulary (build-vocabulary-with-unk pathnames))
         (dictionary (build-dictionary pathnames vocabulary))
         (n-gram-model (build-n-gram-model pathnames vocabulary))
         (unknown-word-vocabulary (build-unknown-word-vocabulary
                                   pathnames
                                   vocabulary))
         (unknown-word-n-gram-model (build-unknown-word-n-gram-model
                                     pathnames
                                     vocabulary
                                     unknown-word-vocabulary)))
    (hachee.kkc.full:make-kkc
     :vocabulary vocabulary
     :n-gram-model n-gram-model
     :vocabulary-dictionary dictionary
     :extended-dictionary (or extended-dictionary
                              (hachee.kkc.word.dictionary:make-dictionary))
     :word-dictionary (or word-dictionary
                          (hachee.kkc.word.dictionary:make-dictionary))
     :tankan-dictionary (or tankan-dictionary
                            (hachee.kkc.word.dictionary:make-dictionary))
     :unknown-word-vocabulary unknown-word-vocabulary
     :unknown-word-n-gram-model unknown-word-n-gram-model
     :sum-probabilities-of-vocabulary-words
     (hachee.kkc.full:sum-probabilities-of-words
      unknown-word-vocabulary
      unknown-word-n-gram-model
      (hachee.kkc.word.dictionary:list-all dictionary)))))
