(defpackage :hachee.kkc.convert
  (:use :cl)
  (:export :make-entry
           :entry-origin
           :entry-form
           :entry-pron
           :entry-token
           :entry-unit

           :compute-score
           :make-score-calc-dto

           :convert-n-gram-model
           :convert-vocabulary
           :convert-word-dictionary
           :convert-sum-probabilities-of-vocabulary-words
           :convert-unknown-word-vocabulary
           :convert-unknown-word-n-gram-model
           :convert-extended-dictionary
           :execute))
(in-package :hachee.kkc.convert)

(defstruct entry
  unit token origin)

(defun entry-form (entry)
  (hachee.kkc.dictionary:unit-form (entry-unit entry)))

(defun entry-pron (entry)
  (hachee.kkc.dictionary:unit-pron (entry-unit entry)))

;;;

(defun from-vocabulary-p (entry)
  (eql (entry-origin entry)
       hachee.kkc.origin:+vocabulary+))

(defun from-extended-dictionary-p (entry)
  (eql (entry-origin entry)
       hachee.kkc.origin:+extended-dictionary+))

(defun make-unknown-word-unit (pron)
  (hachee.kkc.dictionary:make-unit
   :pron pron
   :form (hachee.ja:hiragana->katakana pron)))

(defstruct score-calc-dto
  n-gram-model
  unknown-word-vocabulary
  unknown-word-n-gram-model
  sum-probabilities-of-vocabulary-words
  extended-dictionary)

(defun unknown-word-log-probability (score-calc-dto entry)
  (with-accessors ((unknown-word-vocabulary
                    score-calc-dto-unknown-word-vocabulary)
                   (unknown-word-n-gram-model
                    score-calc-dto-unknown-word-n-gram-model)
                   (sum-probabilities-of-vocabulary-words
                    score-calc-dto-sum-probabilities-of-vocabulary-words)
                   (extended-dictionary
                    score-calc-dto-extended-dictionary))
      score-calc-dto
    (let ((pron-bos-token (hachee.language-model.vocabulary:to-int
                           unknown-word-vocabulary
                           hachee.language-model.vocabulary:+BOS+))
          (pron-eos-token (hachee.language-model.vocabulary:to-int
                           unknown-word-vocabulary
                           hachee.language-model.vocabulary:+EOS+))
          (pron-sentence (hachee.kkc.util:unit->sentence
                          (entry-unit entry)
                          unknown-word-vocabulary)))
      (let ((log-prob-by-unknown-word-n-gram
             (hachee.language-model.n-gram:sentence-log-probability
              unknown-word-n-gram-model pron-sentence
              :BOS pron-bos-token
              :EOS pron-eos-token))
            (extended-dictionary-size
             (hachee.kkc.dictionary:size extended-dictionary)))
        (if (and (from-extended-dictionary-p entry)
                 (< 0 extended-dictionary-size)
                 (< 0 sum-probabilities-of-vocabulary-words))
            (let ((probability-for-extended-dictionary-words
                   (/ sum-probabilities-of-vocabulary-words
                      extended-dictionary-size)))
              (log (+ (exp log-prob-by-unknown-word-n-gram)
                      probability-for-extended-dictionary-words)))
            log-prob-by-unknown-word-n-gram)))))
  
(defun compute-score (score-calc-dto curr-entry prev-entry)
  (let ((curr-token (entry-token curr-entry))
        (prev-token (entry-token prev-entry)))
    (let ((p (hachee.language-model.n-gram:transition-probability
              (score-calc-dto-n-gram-model score-calc-dto)
              curr-token
              (list prev-token))))
      (cond ((= p 0)
             ;; The n-gram model was not able to predict the current token
             ;; For example, if the current token is unknown, and the model
             ;; can't predict unknown tokens, the probability will be 0.
             -10000)
            ((from-vocabulary-p curr-entry)
             (log p))
            (t
             (+ (log p) (unknown-word-log-probability
                         score-calc-dto curr-entry)))))))

;;;

(defstruct list-dto dictionaries vocabulary)

(defun list-entries (list-dto sub-pron)
  (let ((entries nil))
    (with-accessors ((vocabulary list-dto-vocabulary)
                     (dictionaries list-dto-dictionaries)) list-dto
      ;; Add entries from dictionaries
      (dolist (dict dictionaries)
        (dolist (dict-entry (hachee.kkc.dictionary:lookup dict sub-pron))
          (let* ((unit (hachee.kkc.dictionary:entry-unit dict-entry))
                 (token (hachee.language-model.vocabulary:to-int-or-unk
                         vocabulary
                         (hachee.kkc.dictionary:unit->key unit)))
                 (origin (hachee.kkc.dictionary:entry-origin dict-entry)))
            (push (make-entry :unit unit :token token :origin origin)
                  entries))))
      ;; Add unknown word entry if necessary
      (when (< (length sub-pron) 8) ;; Length up to 8
        (let ((unk-unit (make-unknown-word-unit sub-pron)))
          (when (not (some (lambda (dict)
                             (hachee.kkc.dictionary:contains-p
                              dict
                              unk-unit))
                           dictionaries))
            (let ((token (hachee.language-model.vocabulary:to-int
                          vocabulary
                          hachee.language-model.vocabulary:+UNK+))
                  (origin hachee.kkc.origin:+out-of-dictionary+))
              (push (make-entry :unit unk-unit :token token :origin origin)
                    entries))))))
    (nreverse entries)))

;;;

(defgeneric convert-n-gram-model (mixin))
(defgeneric convert-vocabulary (mixin))
(defgeneric convert-word-dictionary (mixin))
(defgeneric convert-sum-probabilities-of-vocabulary-words (mixin))
(defgeneric convert-unknown-word-vocabulary (mixin))
(defgeneric convert-unknown-word-n-gram-model (mixin))
(defgeneric convert-extended-dictionary (mixin))

(defun execute (convert pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi:execute pronunciation
   :begin-entry (make-entry
                 :unit hachee.language-model.vocabulary:+BOS+
                 :token (hachee.language-model.vocabulary:to-int
                         (convert-vocabulary convert)
                         (hachee.kkc.dictionary:unit->key
                          hachee.language-model.vocabulary:+BOS+))
                 :origin hachee.kkc.origin:+vocabulary+)

   :end-entry (make-entry
               :unit hachee.language-model.vocabulary:+EOS+
               :token (hachee.language-model.vocabulary:to-int
                       (convert-vocabulary convert)
                       (hachee.kkc.dictionary:unit->key
                        hachee.language-model.vocabulary:+EOS+))
               :origin hachee.kkc.origin:+vocabulary+)

   :score-fn
   (let ((score-calc-dto
          (make-score-calc-dto
           :n-gram-model
           (convert-n-gram-model convert)
           :unknown-word-vocabulary
           (convert-unknown-word-vocabulary convert)
           :unknown-word-n-gram-model
           (convert-unknown-word-n-gram-model convert)
           :sum-probabilities-of-vocabulary-words
           (convert-sum-probabilities-of-vocabulary-words convert)
           :extended-dictionary
           (convert-extended-dictionary convert))))
     (lambda (curr-entry prev-entry)
       (compute-score score-calc-dto curr-entry prev-entry)))

   :list-entries-fn
   (let ((list-dto (make-list-dto
                    :vocabulary
                    (convert-vocabulary convert)
                    :dictionaries
                    (list (convert-word-dictionary convert)
                          (convert-extended-dictionary convert)))))
     (lambda (pron)
       (list-entries list-dto pron)))

   :1st-boundary-index 1st-boundary-index))
