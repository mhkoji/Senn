(in-package :hachee.kkc)

(defmethod hachee.kkc.lookup:lookup-score-fn ((kkc kkc) prev-unit next-unit)
  (labels ((unit->entry (unit origin)
             (hachee.kkc.convert:make-entry
              :unit unit
              :token (hachee.language-model.vocabulary:to-int-or-unk
                      (kkc-vocabulary kkc)
                      (hachee.kkc.dictionary:unit->key unit))
              :origin origin)))
    (let ((prev-entry (unit->entry
                       prev-unit
                       hachee.kkc.origin:+runtime-none+))
          (next-entry (unit->entry
                       next-unit
                       hachee.kkc.origin:+runtime-none+))
          (score-calc-dto (make-score-calc-dto
                           :n-gram-model
                           (kkc-n-gram-model kkc)
                           :unknown-word-vocabulary
                           (kkc-unknown-word-vocabulary kkc)
                           :unknown-word-n-gram-model
                           (kkc-unknown-word-n-gram-model kkc)
                           :sum-probabilities-of-vocabulary-words
                           (kkc-sum-probabilities-of-vocabulary-words kkc)
                           :extended-dictionary *empty-dictionary*))
          (score-cache (make-hash-table :test #'equal)))
      (lambda (curr-item)
        (let ((curr-entry (unit->entry
                           (hachee.kkc.lookup:item-unit curr-item)
                           (hachee.kkc.lookup:item-origin curr-item)))
              (key (hachee.kkc.dictionary:unit->key
                    (hachee.kkc.lookup:item-unit curr-item))))
          (or (gethash key score-cache)
              (setf (gethash key score-cache)
                    (+ (compute-convert-score
                        score-calc-dto curr-entry prev-entry)
                       (compute-convert-score
                        score-calc-dto next-entry curr-entry)))))))))

(defmethod hachee.kkc.lookup:lookup-word-dictionary ((kkc kkc))
  (kkc-word-dictionary kkc))

(defmethod hachee.kkc.lookup:lookup-char-dictionary ((kkc kkc))
  (kkc-char-dictionary kkc))
