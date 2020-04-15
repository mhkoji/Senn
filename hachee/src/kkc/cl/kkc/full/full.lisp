(defpackage :hachee.kkc.full
  (:use :cl)
  (:import-from :hachee.language-model.vocabulary
                :to-int)
  (:export :kkc
           :make-kkc
           :save-kkc
           :load-kkc
           :sum-probabilities-of-vocabulary-words))
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
   :unknown-word-char-vocabulary (kkc-unknown-word-vocabulary kkc)
   :unknown-word-char-n-gram-model (kkc-unknown-word-n-gram-model kkc)
   :probability-for-extended-dictionary-words
   (let ((extended-dictionary-size
          (hachee.kkc.dictionary:size (kkc-extended-dictionary kkc)))
         (sum-probabilities-of-vocabulary-words
          (kkc-sum-probabilities-of-vocabulary-words kkc)))
     (if (< 0 extended-dictionary-size)
         (/ sum-probabilities-of-vocabulary-words
            extended-dictionary-size)
         0))))

;;; Save
(defun save-kkc (kkc pathname)
  (hachee.kkc.archive:save
   pathname
   :n-gram-model (kkc-n-gram-model kkc)
   :vocabulary (kkc-vocabulary kkc)
   :word-dictionary (kkc-word-dictionary kkc)
   :char-dictionary (kkc-char-dictionary kkc)
   :extended-dictionary (kkc-extended-dictionary kkc)
   :unknown-word-vocabulary (kkc-unknown-word-vocabulary kkc)
   :unknown-word-n-gram-model (kkc-unknown-word-n-gram-model kkc)))


;;; Load
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

(defun load-kkc (pathname)
  (destructuring-bind (&key n-gram-model
                            vocabulary
                            word-dictionary
                            char-dictionary
                            extended-dictionary
                            unknown-word-vocabulary
                            unknown-word-n-gram-model)
      (hachee.kkc.archive:load pathname)
    (make-kkc
     :n-gram-model n-gram-model
     :vocabulary vocabulary
     :word-dictionary word-dictionary
     :char-dictionary char-dictionary
     :extended-dictionary extended-dictionary
     :unknown-word-vocabulary unknown-word-vocabulary
     :unknown-word-n-gram-model unknown-word-n-gram-model
     :sum-probabilities-of-vocabulary-words
     (sum-probabilities-of-vocabulary-words unknown-word-vocabulary
                                            unknown-word-n-gram-model
                                            word-dictionary))))
