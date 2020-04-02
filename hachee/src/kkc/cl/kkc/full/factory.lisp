(defpackage :hachee.kkc.full.factory
  (:use :cl)
  (:import-from :hachee.kkc.build
                :build-vocabulary-with-unk
                :extend-existing-vocabulary
                :build-dictionary
                :build-classifier
                :train-n-gram-model
                :build-unknown-word-vocabulary
                :build-unknown-word-n-gram-model)
  (:export :unk-supported))
(in-package :hachee.kkc.full.factory)

(defun unk-supported (pathnames-segmented
                      &key pathnames-inaccurately-segmented
                           trusted-word-dictionary
                           word-dictionary
                           tankan-dictionary
                           extended-dictionary
                           class-token-to-word-file-path)
  (let ((vocabulary (build-vocabulary-with-unk pathnames-segmented)))
    (when (and pathnames-inaccurately-segmented
               trusted-word-dictionary
               ;; Unable to map an added word to a class
               (not class-token-to-word-file-path))
      (extend-existing-vocabulary vocabulary
                                  trusted-word-dictionary
                                  pathnames-inaccurately-segmented))
    (let ((pathnames
           (append pathnames-segmented
                   pathnames-inaccurately-segmented))
          (n-gram-model
           (if class-token-to-word-file-path
                 (make-instance 'hachee.language-model.n-gram:class-model
                                :classifier (build-classifier
                                             class-token-to-word-file-path
                                             vocabulary)
                                :weights (list 0.115267 0.884733))
                 (make-instance 'hachee.language-model.n-gram:model
                                :weights (list 0.253401 0.746599)))))
      (train-n-gram-model n-gram-model pathnames vocabulary)
      (let* ((dictionary
              (build-dictionary pathnames vocabulary))
             (unknown-word-vocabulary
              (build-unknown-word-vocabulary pathnames
                                             vocabulary))
             (unknown-word-n-gram-model
              (build-unknown-word-n-gram-model pathnames
                                               vocabulary
                                               unknown-word-vocabulary)))
        (hachee.kkc.full:make-kkc
         :vocabulary vocabulary
         :n-gram-model n-gram-model
         :vocabulary-dictionary dictionary
         :extended-dictionary
         (or extended-dictionary
             (hachee.kkc.word.dictionary:make-dictionary))
         :word-dictionary
         (or word-dictionary
             (hachee.kkc.word.dictionary:make-dictionary))
         :tankan-dictionary
         (or tankan-dictionary
             (hachee.kkc.word.dictionary:make-dictionary))
         :unknown-word-vocabulary unknown-word-vocabulary
         :unknown-word-n-gram-model unknown-word-n-gram-model
         :sum-probabilities-of-vocabulary-words
         (hachee.kkc.full:sum-probabilities-of-words
          unknown-word-vocabulary
          unknown-word-n-gram-model
          (hachee.kkc.word.dictionary:list-all dictionary)))))))
