(in-package :hachee.kkc)

;;;

(defun from-vocabulary-p (entry)
  (eql (hachee.kkc.convert:entry-origin entry)
       hachee.kkc.origin:+vocabulary+))

(defun from-extended-dictionary-p (entry)
  (eql (hachee.kkc.convert:entry-origin entry)
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
                          (hachee.kkc.convert:entry-unit entry)
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
  
(defun compute-convert-score (score-calc-dto curr-entry prev-entry)
  (let ((curr-token (hachee.kkc.convert:entry-token curr-entry))
        (prev-token (hachee.kkc.convert:entry-token prev-entry)))
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
            (push (hachee.kkc.convert:make-entry
                   :unit unit :token token :origin origin)
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
              (push (hachee.kkc.convert:make-entry
                     :unit unk-unit :token token :origin origin)
                    entries))))))
    (nreverse entries)))

;;;

(defvar *empty-dictionary*
  (hachee.kkc.dictionary:make-dictionary))

(defmethod hachee.kkc.convert:convert-begin-entry ((kkc kkc))
  (hachee.kkc.convert:make-entry
   :unit hachee.language-model.vocabulary:+BOS+
   :token (hachee.language-model.vocabulary:to-int
           (kkc-vocabulary kkc)
           (hachee.kkc.dictionary:unit->key
            hachee.language-model.vocabulary:+BOS+))
   :origin hachee.kkc.origin:+vocabulary+))

(defmethod hachee.kkc.convert:convert-end-entry ((kkc kkc))
  (hachee.kkc.convert:make-entry
   :unit hachee.language-model.vocabulary:+EOS+
   :token (hachee.language-model.vocabulary:to-int
           (kkc-vocabulary kkc)
           (hachee.kkc.dictionary:unit->key
            hachee.language-model.vocabulary:+EOS+))
   :origin hachee.kkc.origin:+vocabulary+))

(defmethod hachee.kkc.convert:convert-score-fn ((kkc kkc))
  (let ((score-calc-dto (make-score-calc-dto
                         :n-gram-model
                         (kkc-n-gram-model kkc)
                         :unknown-word-vocabulary
                         (kkc-unknown-word-vocabulary kkc)
                         :unknown-word-n-gram-model
                         (kkc-unknown-word-n-gram-model kkc)
                         :sum-probabilities-of-vocabulary-words
                         (kkc-sum-probabilities-of-vocabulary-words kkc)
                         :extended-dictionary *empty-dictionary*)))
    (lambda (curr-entry prev-entry)
      (compute-convert-score score-calc-dto curr-entry prev-entry))))

(defmethod hachee.kkc.convert:convert-list-entries-fn ((kkc kkc))
  (let ((list-dto (make-list-dto
                   :vocabulary (kkc-vocabulary kkc)
                   :dictionaries (list (kkc-word-dictionary kkc)))))
    (lambda (pron)
      (list-entries list-dto pron))))

;;;

(defmethod hachee.kkc.convert:convert-begin-entry ((c kkc-convert))
  (hachee.kkc.convert:convert-begin-entry (kkc-convert-kkc c)))

(defmethod hachee.kkc.convert:convert-end-entry ((c kkc-convert))
  (hachee.kkc.convert:convert-end-entry (kkc-convert-kkc c)))

(defmethod hachee.kkc.convert:convert-score-fn ((c kkc-convert))
  (let ((score-calc-dto (make-score-calc-dto
                         :n-gram-model
                         (kkc-n-gram-model (kkc-convert-kkc c))
                         :unknown-word-vocabulary
                         (kkc-unknown-word-vocabulary (kkc-convert-kkc c))
                         :unknown-word-n-gram-model
                         (kkc-unknown-word-n-gram-model (kkc-convert-kkc c))
                         :sum-probabilities-of-vocabulary-words
                         (kkc-sum-probabilities-of-vocabulary-words
                          (kkc-convert-kkc c))
                         :extended-dictionary
                         (kkc-convert-extended-dictionary c))))
    (lambda (curr-entry prev-entry)
      (compute-convert-score score-calc-dto curr-entry prev-entry))))

(defmethod hachee.kkc.convert:convert-list-entries-fn ((c kkc-convert))
  (let ((list-dto (make-list-dto
                   :vocabulary
                   (kkc-vocabulary (kkc-convert-kkc c))
                   :dictionaries
                   (list (kkc-word-dictionary (kkc-convert-kkc c))
                         (kkc-convert-extended-dictionary c)))))
    (lambda (pron)
      (list-entries list-dto pron))))
