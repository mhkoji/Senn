(defpackage :hachee.kkc.full
  (:use :cl)
  (:import-from :hachee.language-model.vocabulary
                :to-int)
  (:export :kkc
           :make-kkc
           :save-kkc
           :load-kkc
           :sum-probabilities-of-words))
(in-package :hachee.kkc.full)

(defstruct (kkc (:include hachee.kkc:kkc))
  sum-probabilities-of-vocabulary-words
  unknown-word-vocabulary
  unknown-word-n-gram-model)

;;; Convert
(defmethod hachee.kkc:get-convert-score-fn ((kkc kkc))
  (hachee.kkc.full.score-fns:get-for-conv
   :word-vocabulary (kkc-vocabulary kkc)
   :word-n-gram-model (kkc-n-gram-model kkc)
   :unknown-word-char-vocabulary (kkc-unknown-word-vocabulary kkc)
   :unknown-word-char-n-gram-model (kkc-unknown-word-n-gram-model kkc)
   :probability-for-extended-dictionary-words
   (let ((extended-dictionary-size
          (hachee.kkc.word.dictionary:size (kkc-extended-dictionary kkc)))
         (sum-probabilities-of-vocabulary-words
          (kkc-sum-probabilities-of-vocabulary-words kkc)))
     (if (< 0 extended-dictionary-size)
         (/ sum-probabilities-of-vocabulary-words
            extended-dictionary-size)
         0))))


;;; Lookup
(defmethod hachee.kkc:get-lookup-score-fn ((kkc kkc) prev-word next-word)
  (hachee.kkc.full.score-fns:get-for-lookup prev-word next-word
   :word-vocabulary (kkc-vocabulary kkc)
   :word-n-gram-model (kkc-n-gram-model kkc)
   :unknown-word-char-vocabulary (kkc-unknown-word-vocabulary kkc)
   :unknown-word-char-n-gram-model (kkc-unknown-word-n-gram-model kkc)
   :probability-for-extended-dictionary-words
   (let ((extended-dictionary-size
          (hachee.kkc.word.dictionary:size (kkc-extended-dictionary kkc)))
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
   :vocabulary-dictionary (kkc-vocabulary-dictionary kkc)
   :extended-dictionary (kkc-extended-dictionary kkc)
   :word-dictionary (kkc-word-dictionary kkc)
   :tankan-dictionary (kkc-tankan-dictionary kkc)
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
                (hachee.kkc.util:word->sentence word unknown-word-vocabulary)
                :BOS bos-token
                :EOS eos-token)))))

(defun load-kkc (pathname)
  (destructuring-bind (&key n-gram-model
                            vocabulary
                            vocabulary-dictionary
                            extended-dictionary
                            word-dictionary
                            tankan-dictionary
                            unknown-word-vocabulary
                            unknown-word-n-gram-model)
      (hachee.kkc.archive:load pathname)
    (make-kkc
     :n-gram-model n-gram-model
     :vocabulary vocabulary
     :vocabulary-dictionary vocabulary-dictionary
     :extended-dictionary extended-dictionary
     :word-dictionary word-dictionary
     :tankan-dictionary tankan-dictionary
     :unknown-word-vocabulary unknown-word-vocabulary
     :unknown-word-n-gram-model unknown-word-n-gram-model
     :sum-probabilities-of-vocabulary-words
     (sum-probabilities-of-words
      unknown-word-vocabulary
      unknown-word-n-gram-model
      (hachee.kkc.word.dictionary:list-all vocabulary-dictionary)))))