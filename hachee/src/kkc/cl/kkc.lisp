(defpackage :hachee.kkc
  (:use :cl)
  (:export :kkc
           :save-kkc
           :load-kkc
           :build-kkc
           :build-kkc-simple

           :kkc-convert
           :make-kkc-convert))
(in-package :hachee.kkc)

(defstruct kkc
  n-gram-model
  vocabulary
  word-dictionary
  char-dictionary
  sum-probabilities-of-vocabulary-words
  unknown-word-vocabulary
  unknown-word-n-gram-model)

(defstruct kkc-convert
  kkc
  extended-dictionary)


(defmethod hachee.kkc.persist:save-object
    ((obj hachee.language-model.n-gram:model) s)
  (hachee.language-model.n-gram:save-model obj s))

(defmethod hachee.kkc.persist:save-object
    ((obj hachee.kkc.dictionary:dictionary) s)
  (hachee.kkc.dictionary:save-dictionary obj s))

(defmethod hachee.kkc.persist:save-object
    ((obj hachee.language-model.vocabulary:vocabulary) s)
  (hachee.language-model.vocabulary:save-vocabulary obj s))

(defmethod hachee.kkc.persist:load-object
    ((type (eql 'hachee.language-model.n-gram:model)) s)
  (hachee.language-model.n-gram:load-model type s))

(defmethod hachee.kkc.persist:load-object
    ((type (eql 'hachee.language-model.n-gram:class-model)) s)
  (hachee.language-model.n-gram:load-model type s))

(defmethod hachee.kkc.persist:load-object
    ((type (eql 'hachee.kkc.dictionary:dictionary)) s)
  (hachee.kkc.dictionary:load-dictionary s))

(defmethod hachee.kkc.persist:load-object
    ((type (eql 'hachee.language-model.vocabulary:vocabulary)) s)
  (hachee.language-model.vocabulary:load-vocabulary s))

(defun save-kkc (kkc pathname)
  (hachee.kkc.persist:do-save-into-zip (add-entry pathname)
    (add-entry "n-gram-model.txt"
               (kkc-n-gram-model kkc)
               (if (typep (kkc-n-gram-model kkc)
                          'hachee.language-model.n-gram:class-model)
                   'hachee.language-model.n-gram:class-model
                   'hachee.language-model.n-gram:model))
    (add-entry "vocabulary.txt"
               (kkc-vocabulary kkc))
    (add-entry "word-dictionary.txt"
               (kkc-word-dictionary kkc))
    (add-entry "char-dictionary.txt"
               (kkc-char-dictionary kkc))
    (add-entry "unknown-word-vocabulary.txt"
               (kkc-unknown-word-vocabulary kkc))
    (add-entry "unknown-word-n-gram-model.txt"
               (kkc-unknown-word-n-gram-model kkc)))
  (values))

(defun sum-probabilities-of-words (unknown-word-vocabulary
                                   unknown-word-n-gram-model
                                   words)
  (let ((bos-token (hachee.language-model.vocabulary:to-int
                    unknown-word-vocabulary
                    hachee.language-model.vocabulary:+BOS+))
        (eos-token (hachee.language-model.vocabulary:to-int
                    unknown-word-vocabulary
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
           (remove-if-not
            (lambda (e)
              (eql (hachee.kkc.dictionary:entry-origin e)
                   hachee.kkc.origin:+vocabulary+))
            (hachee.kkc.dictionary:list-all word-dictionary)))))

(defun load-kkc (pathname)
  (let ((entry-alist (hachee.kkc.persist:load-from-zip pathname)))
    (labels ((ensure-not-null (x)
               (assert x)
               x)
             (get-entry (filename)
               (ensure-not-null
                (cdr (assoc filename entry-alist :test #'string=)))))
      (let ((unknown-word-vocabulary
             (get-entry "unknown-word-vocabulary.txt"))
            (unknown-word-n-gram-model
             (get-entry "unknown-word-n-gram-model.txt"))
            (word-dictionary
             (get-entry "word-dictionary.txt")))
        (make-kkc
         :n-gram-model
         (get-entry "n-gram-model.txt")
         :vocabulary
         (get-entry "vocabulary.txt")
         :word-dictionary word-dictionary
         :char-dictionary
         (get-entry "char-dictionary.txt")
         :unknown-word-vocabulary unknown-word-vocabulary
         :unknown-word-n-gram-model unknown-word-n-gram-model
         :sum-probabilities-of-vocabulary-words
         (sum-probabilities-of-vocabulary-words unknown-word-vocabulary
                                                unknown-word-n-gram-model
                                                word-dictionary))))))

(defun build-kkc-simple (pathnames &key char-dictionary)
  (let ((vocabulary (hachee.kkc.build:build-vocabulary pathnames))
        (n-gram-model (make-instance 'hachee.language-model.n-gram:model)))
    (hachee.kkc.build:train-n-gram-model n-gram-model pathnames vocabulary)
    (make-kkc
     :n-gram-model n-gram-model
     :vocabulary vocabulary
     :word-dictionary
     (hachee.kkc.build:build-word-dictionary pathnames vocabulary)
     :char-dictionary (or char-dictionary
                          (hachee.kkc.dictionary:make-dictionary))
     :unknown-word-vocabulary
     (hachee.language-model.vocabulary:make-vocabulary)
     :unknown-word-n-gram-model
     (make-instance 'hachee.language-model.n-gram:model)
     :sum-probabilities-of-vocabulary-words 0)))

(defun build-kkc (pathnames-segmented
                  &key pathnames-inaccurately-segmented
                       word-dictionary-pathnames
                       char-dictionary
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
                              :classifier (hachee.kkc.build:build-classifier
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
         :unknown-word-vocabulary unknown-word-vocabulary
         :unknown-word-n-gram-model unknown-word-n-gram-model
         :sum-probabilities-of-vocabulary-words
         (sum-probabilities-of-vocabulary-words unknown-word-vocabulary
                                                unknown-word-n-gram-model
                                                word-dictionary))))))

