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
                    score-calc-dto-unknown-word-n-gram-model)) score-calc-dto
    (let ((sentence (hachee.kkc.util:unit->sentence
                     (hachee.kkc.convert:entry-unit entry)
                     unknown-word-vocabulary))
          (bos (hachee.language-model.vocabulary:to-int
                unknown-word-vocabulary
                hachee.language-model.vocabulary:+BOS+))
          (eos (hachee.language-model.vocabulary:to-int
                unknown-word-vocabulary
                 hachee.language-model.vocabulary:+EOS+)))
      (hachee.language-model.n-gram:sentence-log-probability
       unknown-word-n-gram-model sentence :BOS bos :EOS eos))))

(defun extended-dictionary-word-probability (score-calc-dto entry)
  (with-accessors ((sum-probabilities-of-vocabulary-words
                    score-calc-dto-sum-probabilities-of-vocabulary-words)
                   (extended-dictionary
                    score-calc-dto-extended-dictionary)) score-calc-dto
    (let ((extended-dictionary-size
           (hachee.kkc.dictionary:size extended-dictionary)))
      (if (and (from-extended-dictionary-p entry)
               (< 0 extended-dictionary-size)
               (< 0 sum-probabilities-of-vocabulary-words))
          (/ sum-probabilities-of-vocabulary-words
             extended-dictionary-size)
          0))))

(defun transit-probability (score-calc-dto curr-entry prev-entry)
  (let ((curr-token (hachee.kkc.convert:entry-token curr-entry))
        (prev-token (hachee.kkc.convert:entry-token prev-entry))
        (n-gram-model (score-calc-dto-n-gram-model score-calc-dto)))
    (hachee.language-model.n-gram:transition-probability
     n-gram-model curr-token (list prev-token))))

(defun compute-convert-score (score-calc-dto curr-entry prev-entry)
  (let ((prob-transit (transit-probability
                       score-calc-dto curr-entry prev-entry)))
    (cond ((= prob-transit 0)
           ;; The n-gram model was not able to predict the current token
           ;; For example, if the current token is unknown, and the model
           ;; can't predict unknown tokens, the probability will be 0.
           -10000)
          ((from-vocabulary-p curr-entry)
           (log prob-transit))
          (t
           (+ (log prob-transit)
              (let ((log-prob-unknown (unknown-word-log-probability
                                       score-calc-dto curr-entry))
                    (prob-extended (extended-dictionary-word-probability
                                    score-calc-dto curr-entry)))
                (if (< 0 prob-extended)
                    (log (+ (exp log-prob-unknown)
                            prob-extended))
                    log-prob-unknown)))))))

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
           hachee.language-model.vocabulary:+BOS+)
   :origin hachee.kkc.origin:+vocabulary+))

(defmethod hachee.kkc.convert:convert-end-entry ((kkc kkc))
  (hachee.kkc.convert:make-entry
   :unit hachee.language-model.vocabulary:+EOS+
   :token (hachee.language-model.vocabulary:to-int
           (kkc-vocabulary kkc)
           hachee.language-model.vocabulary:+EOS+)
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
