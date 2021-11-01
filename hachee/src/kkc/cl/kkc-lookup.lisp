(in-package :hachee.kkc)

(defmethod hachee.kkc.lookup:lookup-score-fn ((kkc kkc)
                                              prev-unit next-unit)
  (labels ((unit->entry (unit origin)
             (hachee.kkc.convert:make-entry
              :unit unit
              :token (hachee.language-model.vocabulary:to-int-or-unk
                      (kkc-vocabulary kkc)
                      unit)
              :origin origin))
           (compute-score (curr-entry prev-entry)
             (hachee.kkc.convert:compute-score
              curr-entry
              prev-entry
              (kkc-n-gram-model kkc)
              (kkc-unknown-word-vocabulary kkc)
              (kkc-unknown-word-n-gram-model kkc)
              (kkc-sum-probabilities-of-vocabulary-words kkc)
              *empty-dictionary*)))
    (let ((prev-entry (unit->entry prev-unit
                                   hachee.kkc.origin:+runtime-none+))
          (next-entry (unit->entry next-unit
                                   hachee.kkc.origin:+runtime-none+))
          (score-cache (make-hash-table :test #'equal)))
      (lambda (curr-item)
        (let ((curr-entry (unit->entry
                           (hachee.kkc.lookup:item-unit curr-item)
                           (hachee.kkc.lookup:item-origin curr-item)))
              (key (hachee.kkc.dictionary:unit->key
                    (hachee.kkc.lookup:item-unit curr-item))))
          (or (gethash key score-cache)
              (setf (gethash key score-cache)
                    (+ (compute-score curr-entry prev-entry)
                       (compute-score next-entry curr-entry)))))))))
                        


(defmethod hachee.kkc.lookup:lookup-word-dictionary ((kkc kkc))
  (kkc-word-dictionary kkc))

(defmethod hachee.kkc.lookup:lookup-char-dictionary ((kkc kkc))
  (kkc-char-dictionary kkc))
