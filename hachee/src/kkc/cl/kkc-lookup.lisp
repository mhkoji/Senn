(in-package :hachee.kkc)

(defmethod hachee.kkc.lookup:lookupper-score-fn ((kkc kkc)
                                                 prev-unit next-unit)
  (labels ((unit->entry (unit)
             (hachee.kkc.convert:make-entry
              :unit unit
              :token (hachee.language-model.vocabulary:to-int-or-unk
                      (hachee.kkc:kkc-vocabulary kkc)
                      unit)
              :origin hachee.kkc.origin:+runtime-none+)))
    (let ((prev-entry (unit->entry prev-unit))
          (next-entry (unit->entry next-unit))
          (score-cache (make-hash-table :test #'equal)))
      (lambda (curr-item)
        (let ((unit (hachee.kkc.lookup:item-unit curr-item))
              (origin (hachee.kkc.lookup:item-origin curr-item)))
          (let ((curr-entry
                 (hachee.kkc.convert:make-entry
                  :unit unit
                  :token (hachee.language-model.vocabulary:to-int-or-unk
                          (hachee.kkc:kkc-vocabulary kkc)
                          unit)
                  :origin origin))
                (key
                 (hachee.kkc.dictionary:unit->key unit)))
            (or (gethash key score-cache)
                (setf (gethash key score-cache)
                      (+ (compute-convert-score
                          kkc curr-entry prev-entry)
                         (compute-convert-score
                          kkc next-entry curr-entry))))))))))

(defmethod hachee.kkc.lookup:lookupper-word-dictionary ((kkc kkc))
  (kkc-word-dictionary kkc))

(defmethod hachee.kkc.lookup:lookupper-char-dictionary ((kkc kkc))
  (kkc-char-dictionary kkc))
                                                        
