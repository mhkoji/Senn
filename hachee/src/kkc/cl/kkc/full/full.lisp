(defpackage :hachee.kkc.full
  (:use :cl)
  (:import-from :hachee.language-model.vocabulary
                :to-int)
  (:export :kkc
           :make-from-corpora))
(in-package :hachee.kkc.full)

(defstruct (kkc (:include hachee.kkc:kkc))
  sum-probabilities-of-vocabulary-words
  unknown-word-vocabulary
  unknown-word-n-gram-model)


;;; Convert
(defmethod hachee.kkc:get-convert-score-fn ((kkc kkc))
  (hachee.kkc.full.score-fns:get-for-conv
   :word-n-gram-model (kkc-n-gram-model kkc)
   :unknown-word-log-probability-fn
   (let ((extended-dictionary-size
          (hachee.kkc.dictionary:size (kkc-extended-dictionary kkc)))
         (sum-probabilities-of-vocabulary-words
          (kkc-sum-probabilities-of-vocabulary-words kkc)))
     (lambda (entry)
       (hachee.kkc.full.score-fns:unknown-word-log-probability
        entry
        (kkc-unknown-word-vocabulary kkc)
        (kkc-unknown-word-n-gram-model kkc)
        (if (< 0 extended-dictionary-size)
            (/ sum-probabilities-of-vocabulary-words
               extended-dictionary-size)
            0))))))


;;; Lookup
(defmethod hachee.kkc:get-lookup-score-fn ((kkc kkc) prev-word next-word)
  (hachee.kkc.full.score-fns:get-for-lookup prev-word next-word
   :word-vocabulary (kkc-vocabulary kkc)
   :word-n-gram-model (kkc-n-gram-model kkc)
   :unknown-word-pron-vocabulary (kkc-unknown-word-vocabulary kkc)
   :unknown-word-pron-n-gram-model (kkc-unknown-word-n-gram-model kkc)
   :probability-for-extended-dictionary-words
   (let ((extended-dictionary-size
          (hachee.kkc.dictionary:size (kkc-extended-dictionary kkc)))
         (sum-probabilities-of-vocabulary-words
          (kkc-sum-probabilities-of-vocabulary-words kkc)))
     (if (< 0 extended-dictionary-size)
         (/ sum-probabilities-of-vocabulary-words
            extended-dictionary-size)
         0))))


;; Save
(defmethod hachee.kkc:save-to-files ((kkc kkc) add-file-fn)
  (let ((n-gram-model (kkc-n-gram-model kkc)))
    (funcall add-file-fn
             (if (typep n-gram-model
                        'hachee.language-model.n-gram:class-model)
                 "class-n-gram-model.txt"
                 "n-gram-model.txt")
             (with-output-to-string (stream)
               (hachee.language-model.n-gram:save-model n-gram-model
                                                        stream))))
  (loop for (filename object)
            in (list (list "vocabulary.txt"
                           (kkc-vocabulary kkc))
                     (list "word-dictionary.txt"
                           (kkc-word-dictionary kkc))
                     (list "char-dictionary.txt"
                           (kkc-char-dictionary kkc))
                     (list "extended-dictionary.txt"
                           (kkc-extended-dictionary kkc))
                     (list "unknown-word-vocabulary.txt"
                           (kkc-unknown-word-vocabulary kkc))
                     (list "unknown-word-n-gram-model.txt"
                           (kkc-unknown-word-n-gram-model kkc)))
        for data-string = (with-output-to-string (s)
                            (hachee.kkc.archive:save-object object s))
        do (progn
             (funcall add-file-fn filename data-string)))
  (values))


;; Load
(defun sum-probabilities-of-words (unknown-word-vocabulary
                                   unknown-word-n-gram-model
                                   words)
  (let ((bos-token (to-int unknown-word-vocabulary
                           hachee.language-model.vocabulary:+BOS+))
        (eos-token (to-int unknown-word-vocabulary
                           hachee.language-model.vocabulary:+EOS+)))
    (loop
      for word in words
      sum (exp (hachee.language-model.n-gram:sentence-log-probability
                unknown-word-n-gram-model
                (hachee.kkc.util:unit->sentence word unknown-word-vocabulary)
                :BOS bos-token
                :EOS eos-token)))))

(defun sum-probabilities-of-vocabulary-words (unknown-word-vocabulary
                                              unknown-word-n-gram-model
                                              word-dictionary)
  (sum-probabilities-of-words
   unknown-word-vocabulary
   unknown-word-n-gram-model
   (mapcar #'hachee.kkc.dictionary:entry-unit
           (remove-if-not (lambda (ent)
                            (eql (hachee.kkc.dictionary:entry-origin ent)
                                 hachee.kkc:+origin-vocabulary+))
                          (hachee.kkc.dictionary:list-all word-dictionary)))))

(defmethod hachee.kkc:make-from-files ((kkc-type (eql 'kkc))
                                       read-from-file-fn)
  (labels ((load-from-file (type filename)
             (funcall read-from-file-fn
                      filename
                      (lambda (s)
                        (hachee.kkc.archive:load-object-as type s))))
           (ensure-not-null (x)
             (assert x)
             x))
    (let ((unknown-word-vocabulary
           (ensure-not-null
            (load-from-file 'hachee.language-model.vocabulary:vocabulary
                            "unknown-word-vocabulary.txt")))
          (unknown-word-n-gram-model
           (ensure-not-null
            (load-from-file 'hachee.language-model.n-gram:model
                            "unknown-word-n-gram-model.txt")))
          (word-dictionary
           (ensure-not-null
            (load-from-file 'hachee.kkc.dictionary:dictionary
                            "word-dictionary.txt"))))
      (make-kkc
       :n-gram-model
       (ensure-not-null
        (or (load-from-file 'hachee.language-model.n-gram:class-model
                            "class-n-gram-model.txt")
            (load-from-file 'hachee.language-model.n-gram:model
                            "n-gram-model.txt")))
       :vocabulary
       (ensure-not-null
        (load-from-file 'hachee.language-model.vocabulary:vocabulary
                        "vocabulary.txt"))
       :word-dictionary word-dictionary
       :char-dictionary
       (ensure-not-null
        (load-from-file 'hachee.kkc.dictionary:dictionary
                        "char-dictionary.txt"))
       :extended-dictionary
       (ensure-not-null
        (load-from-file 'hachee.kkc.dictionary:dictionary
                        "extended-dictionary.txt"))
       :unknown-word-vocabulary unknown-word-vocabulary
       :unknown-word-n-gram-model unknown-word-n-gram-model
       :sum-probabilities-of-vocabulary-words
       (sum-probabilities-of-vocabulary-words unknown-word-vocabulary
                                              unknown-word-n-gram-model
                                              word-dictionary)))))


(defun make-from-corpora (pathnames-segmented
                          &key pathnames-inaccurately-segmented
                               word-dictionary-pathnames
                               char-dictionary
                               extended-dictionary
                               trusted-word-dictionary
                               class-token-to-word-file-path)
  (let ((vocabulary
         (hachee.kkc.build:build-vocabulary-with-unk pathnames-segmented)))
    (when (and pathnames-inaccurately-segmented
               trusted-word-dictionary
               ;; Unable to map an added word to a class
               (not class-token-to-word-file-path))
      (hachee.kkc.build:extend-existing-vocabulary
       vocabulary
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
      (hachee.kkc.build:train-n-gram-model n-gram-model pathnames vocabulary)
      (let* ((word-dictionary
              (hachee.kkc.build:add-to-word-dictionary-from-resources
               (hachee.kkc.build:build-word-dictionary pathnames vocabulary)
               word-dictionary-pathnames))
             (unknown-word-vocabulary
              (hachee.kkc.build:build-unknown-word-vocabulary pathnames
                                                              vocabulary))
             (unknown-word-n-gram-model
              (hachee.kkc.build:build-unknown-word-n-gram-model
               pathnames
               vocabulary
               unknown-word-vocabulary)))
        (make-kkc
         :n-gram-model n-gram-model
         :vocabulary vocabulary
         :word-dictionary word-dictionary
         :char-dictionary (or char-dictionary
                              (hachee.kkc.dictionary:make-dictionary))
         :extended-dictionary (or extended-dictionary
                                  (hachee.kkc.dictionary:make-dictionary))
         :unknown-word-vocabulary unknown-word-vocabulary
         :unknown-word-n-gram-model unknown-word-n-gram-model
         :sum-probabilities-of-vocabulary-words
         (sum-probabilities-of-vocabulary-words unknown-word-vocabulary
                                                unknown-word-n-gram-model
                                                word-dictionary))))))
