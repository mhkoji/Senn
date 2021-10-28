(in-package :hachee.kkc)

(defun make-unknown-word-unit (pron)
  (hachee.kkc.dictionary:make-unit
   :pron pron
   :form (hachee.ja:hiragana->katakana pron)))

(defun unknown-word-log-probability (entry kkc)
  (let ((unknown-word-pron-vocabulary
         (hachee.kkc:kkc-unknown-word-vocabulary kkc))
        (unknown-word-pron-n-gram-model
         (hachee.kkc:kkc-unknown-word-n-gram-model kkc))
        (extended-dictionary-size
         (hachee.kkc.dictionary:size
          (hachee.kkc:kkc-extended-dictionary kkc))))
    (let ((pron-bos-token (hachee.language-model.vocabulary:to-int
                           unknown-word-pron-vocabulary
                           hachee.language-model.vocabulary:+BOS+))
          (pron-eos-token (hachee.language-model.vocabulary:to-int
                           unknown-word-pron-vocabulary
                           hachee.language-model.vocabulary:+EOS+))
          (pron-sentence (hachee.kkc.util:unit->sentence
                          (entry-unit entry)
                          unknown-word-pron-vocabulary)))
      (let ((log-prob-by-unknown-word-n-gram
             (hachee.language-model.n-gram:sentence-log-probability
              unknown-word-pron-n-gram-model pron-sentence
              :BOS pron-bos-token
              :EOS pron-eos-token)))
        (if (and (from-extended-dictionary-p entry)
                 (< 0 extended-dictionary-size))
            (let ((probability-for-extended-dictionary-words
                   (/ (hachee.kkc:kkc-sum-probabilities-of-vocabulary-words
                       kkc)
                      extended-dictionary-size)))
              (log (+ (exp log-prob-by-unknown-word-n-gram)
                      probability-for-extended-dictionary-words)))
            log-prob-by-unknown-word-n-gram)))))

(defun compute-convert-score (kkc curr-entry prev-entry)
  (let ((curr-token (hachee.kkc.convert:entry-token curr-entry))
        (prev-token (hachee.kkc.convert:entry-token prev-entry)))
    (let ((p (hachee.language-model.n-gram:transition-probability
              (hachee.kkc:kkc-n-gram-model kkc)
              curr-token
              (list prev-token))))
      (cond ((= p 0)
             -10000)
            ((from-vocabulary-p curr-entry)
             (log p))
            (t
             (+ (log p)
                (unknown-word-log-probability curr-entry kkc)))))))
  
(defmethod hachee.kkc.convert:converter-begin-entry ((kkc kkc))
  (hachee.kkc.convert:make-entry
   :unit hachee.language-model.vocabulary:+BOS+
   :token (hachee.language-model.vocabulary:to-int
           (hachee.kkc:kkc-vocabulary kkc)
           hachee.language-model.vocabulary:+BOS+)
   :origin hachee.kkc.origin:+vocabulary+))

(defmethod hachee.kkc.convert:converter-end-entry ((kkc kkc))
  (hachee.kkc.convert:make-entry
   :unit hachee.language-model.vocabulary:+EOS+
   :token (hachee.language-model.vocabulary:to-int
           (hachee.kkc:kkc-vocabulary kkc)
           hachee.language-model.vocabulary:+EOS+)
   :origin hachee.kkc.origin:+vocabulary+))

(defmethod hachee.kkc.convert:converter-score-fn ((kkc kkc))
  (lambda (curr-entry prev-entry)
    (compute-convert-score kkc curr-entry prev-entry)))

(defun from-vocabulary-p (entry)
  (eql (hachee.kkc.convert:entry-origin entry)
       hachee.kkc.origin:+vocabulary+))

(defun from-extended-dictionary-p (entry)
  (eql (hachee.kkc.convert:entry-origin entry)
       hachee.kkc.origin:+extended-dictionary+))

(defun list-entries (sub-pron &key dictionaries vocabulary)
  (labels ((dictionary-entry->convert-entry (dictionary-entry)
             (let* ((unit (hachee.kkc.dictionary:entry-unit
                           dictionary-entry))
                    (origin (hachee.kkc.dictionary:entry-origin
                             dictionary-entry))
                    (token (hachee.language-model.vocabulary:to-int-or-unk
                            vocabulary
                            (hachee.kkc.dictionary:unit->key unit))))
               (hachee.kkc.convert:make-entry
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
                             (hachee.kkc.dictionary:contains-p dict unk-unit))
                           dictionaries))
            (push (hachee.kkc.convert:make-entry
                   :unit unk-unit
                   :token (hachee.language-model.vocabulary:to-int
                           vocabulary
                           hachee.language-model.vocabulary:+UNK+)
                   :origin hachee.kkc.origin:+out-of-dictionary+)
                  entries))))
      (nreverse entries))))

(defmethod hachee.kkc.convert:converter-lookup-entries ((kkc kkc)
                                                        (sub-pron string))
  (list-entries sub-pron
                :dictionaries
                (list (kkc-word-dictionary kkc)
                      (kkc-extended-dictionary kkc))
                :vocabulary
                (kkc-vocabulary kkc)))
