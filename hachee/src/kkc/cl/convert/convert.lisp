(defpackage :hachee.kkc.convert
  (:use :cl)
  (:export :make-entry
           :entry-origin
           :entry-form
           :entry-pron
           :entry-token
           :entry-unit

           :compute-score

           :convert-n-gram-model
           :convert-vocabulary
           :convert-word-dictionary
           :convert-char-dictionary
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

(defgeneric convert-n-gram-model (mixin))
(defgeneric convert-vocabulary (mixin))
(defgeneric convert-word-dictionary (mixin))
(defgeneric convert-char-dictionary (mixin))
(defgeneric convert-sum-probabilities-of-vocabulary-words (mixin))
(defgeneric convert-unknown-word-vocabulary (mixin))
(defgeneric convert-unknown-word-n-gram-model (mixin))
(defgeneric convert-extended-dictionary (mixin))

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

(defun unknown-word-log-probability (entry
                                     unknown-word-vocabulary
                                     unknown-word-n-gram-model
                                     sum-probabilities-of-vocabulary-words
                                     extended-dictionary)
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
          log-prob-by-unknown-word-n-gram))))

(defun compute-score (curr-entry
                      prev-entry
                      n-gram-model
                      unknown-word-vocabulary
                      unknown-word-n-gram-model
                      sum-probabilities-of-vocabulary-words
                      extended-dictionary)
  (let ((curr-token (entry-token curr-entry))
        (prev-token (entry-token prev-entry)))
    (let ((p (hachee.language-model.n-gram:transition-probability
              n-gram-model
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
             (+ (log p)
                (unknown-word-log-probability
                 curr-entry
                 unknown-word-vocabulary
                 unknown-word-n-gram-model
                 extended-dictionary
                 sum-probabilities-of-vocabulary-words)))))))

(defun list-entries (sub-pron &key dictionaries vocabulary)
  (labels ((dictionary-entry->convert-entry (dictionary-entry)
             (let* ((unit (hachee.kkc.dictionary:entry-unit
                           dictionary-entry))
                    (origin (hachee.kkc.dictionary:entry-origin
                             dictionary-entry))
                    (token (hachee.language-model.vocabulary:to-int-or-unk
                            vocabulary
                            (hachee.kkc.dictionary:unit->key unit))))
               (make-entry
                :unit unit :token token :origin origin))))
    (let ((entries (alexandria:mappend
                    (lambda (dict)
                      (mapcar #'dictionary-entry->convert-entry
                              (hachee.kkc.dictionary:lookup dict sub-pron)))
                    dictionaries)))
      ;; Add unknown word entry if necessary
      (when (< (length sub-pron) 8) ;; Length up to 8
        (let ((unk-unit (make-unknown-word-unit sub-pron)))
          (when (not (some (lambda (dict)
                             (hachee.kkc.dictionary:contains-p dict
                                                               unk-unit))
                           dictionaries))
            (push (make-entry
                   :unit unk-unit
                   :token (hachee.language-model.vocabulary:to-int
                           vocabulary
                           hachee.language-model.vocabulary:+UNK+)
                   :origin hachee.kkc.origin:+out-of-dictionary+)
                  entries))))
      (nreverse entries))))

(defun execute (convert pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi:execute pronunciation
   :begin-entry (make-entry
                 :unit hachee.language-model.vocabulary:+BOS+
                 :token (hachee.language-model.vocabulary:to-int
                         (convert-vocabulary convert)
                         hachee.language-model.vocabulary:+BOS+)
                 :origin hachee.kkc.origin:+vocabulary+)

   :end-entry (make-entry
               :unit hachee.language-model.vocabulary:+EOS+
               :token (hachee.language-model.vocabulary:to-int
                       (convert-vocabulary convert)
                       hachee.language-model.vocabulary:+EOS+)
               :origin hachee.kkc.origin:+vocabulary+)

   :score-fn (let ((n-gram-model
                    (convert-n-gram-model convert))
                   (unknown-word-vocabulary
                    (convert-unknown-word-vocabulary convert))
                   (unknown-word-n-gram-model
                    (convert-unknown-word-n-gram-model convert))
                   (sum-probabilities-of-vocabulary-words
                    (convert-sum-probabilities-of-vocabulary-words convert))
                   (extended-dictionary
                    (convert-extended-dictionary convert)))
               (lambda (curr-entry prev-entry)
                 (compute-score curr-entry
                                prev-entry
                                n-gram-model
                                unknown-word-vocabulary
                                unknown-word-n-gram-model
                                sum-probabilities-of-vocabulary-words
                                extended-dictionary)))

   :list-entries-fn
   (let ((vocab (convert-vocabulary convert))
         (dicts (list (convert-word-dictionary convert)
                      (convert-extended-dictionary convert))))
     (lambda (pron)
       (list-entries pron :dictionaries dicts :vocabulary vocab)))

   :1st-boundary-index 1st-boundary-index))
