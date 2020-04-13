(defpackage :hachee.kkc.full.factory
  (:use :cl)
  (:import-from :hachee.kkc.build
                :build-vocabulary-with-unk
                :extend-existing-vocabulary
                :add-dictionary-entries-from-files
                :build-classifier
                :train-n-gram-model
                :build-unknown-word-vocabulary
                :build-unknown-word-n-gram-model)
  (:export :unk-supported))
(in-package :hachee.kkc.full.factory)

(defun unk-supported (pathnames-segmented
                      &key pathnames-inaccurately-segmented
                           word-dictionary
                           char-dictionary
                           extended-dictionary
                           trusted-word-dictionary
                           class-token-to-word-file-path)
  (unless word-dictionary
    (setq word-dictionary (hachee.kkc.dictionary:make-dictionary)))
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
      (add-dictionary-entries-from-files word-dictionary pathnames vocabulary)
      (let ((unknown-word-vocabulary
             (build-unknown-word-vocabulary pathnames
                                            vocabulary))
            (unknown-word-n-gram-model
             (build-unknown-word-n-gram-model pathnames
                                              vocabulary
                                              unknown-word-vocabulary)))
        (hachee.kkc.full:make-kkc
         :n-gram-model n-gram-model
         :vocabulary vocabulary
         :word-dictionary dictionary
         :char-dictionary (or char-dictionary
                              (hachee.kkc.dictionary:make-dictionary))
         :extended-dictionary (or extended-dictionary
                                  (hachee.kkc.dictionary:make-dictionary))
         :unknown-word-vocabulary unknown-word-vocabulary
         :unknown-word-n-gram-model unknown-word-n-gram-model
         :sum-probabilities-of-vocabulary-words
         (hachee.kkc.full:sum-probabilities-of-words
          unknown-word-vocabulary
          unknown-word-n-gram-model
          (loop for entries
                    in (hachee.kkc.dictionary:list-all word-dictionary)
                for vocabulary-entries
                    = (remove-if-not
                       (lambda (ent)
                         (eql (hachee.kkc.dictionary:entry-origin ent)
                              hachee.kkc.dictionary:+origin-vocabulary+))
                       entries)
                nconc (mapcar #'hachee.kkc.dictionary:entry-unit
                              vocabulary-entries))))))))
