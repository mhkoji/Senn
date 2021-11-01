(in-package :hachee.kkc)

(defvar *empty-dictionary*
  (hachee.kkc.dictionary:make-dictionary))

(defmethod hachee.kkc.convert:convert-n-gram-model ((kkc kkc))
  (kkc-n-gram-model kkc))

(defmethod hachee.kkc.convert:convert-vocabulary ((kkc kkc))
  (kkc-vocabulary kkc))

(defmethod hachee.kkc.convert:convert-word-dictionary ((kkc kkc))
  (kkc-word-dictionary kkc))

(defmethod hachee.kkc.convert:convert-char-dictionary ((kkc kkc))
  (kkc-char-dictionary kkc))

(defmethod hachee.kkc.convert:convert-sum-probabilities-of-vocabulary-words
    ((kkc kkc))
  (kkc-sum-probabilities-of-vocabulary-words kkc))

(defmethod hachee.kkc.convert:convert-unknown-word-n-gram-model ((kkc kkc))
  (kkc-unknown-word-n-gram-model kkc))

(defmethod hachee.kkc.convert:convert-unknown-word-vocabulary ((kkc kkc))
  (kkc-unknown-word-vocabulary kkc))

(defmethod hachee.kkc.convert:convert-extended-dictionary ((kkc kkc))
  *empty-dictionary*)


(defmethod hachee.kkc.convert:convert-n-gram-model
    ((c kkc-convert))
  (kkc-n-gram-model (kkc-convert-kkc c)))

(defmethod hachee.kkc.convert:convert-vocabulary
    ((c kkc-convert))
  (kkc-vocabulary (kkc-convert-kkc c)))

(defmethod hachee.kkc.convert:convert-word-dictionary
    ((c kkc-convert))
  (kkc-word-dictionary (kkc-convert-kkc c)))

(defmethod hachee.kkc.convert:convert-char-dictionary
    ((c kkc-convert))
  (kkc-char-dictionary (kkc-convert-kkc c)))

(defmethod hachee.kkc.convert:convert-sum-probabilities-of-vocabulary-words
    ((c kkc-convert))
  (kkc-sum-probabilities-of-vocabulary-words (kkc-convert-kkc c)))

(defmethod hachee.kkc.convert:convert-unknown-word-n-gram-model
    ((c kkc-convert))
  (kkc-unknown-word-n-gram-model (kkc-convert-kkc c)))

(defmethod hachee.kkc.convert:convert-unknown-word-vocabulary
    ((c kkc-convert))
  (kkc-unknown-word-vocabulary (kkc-convert-kkc c)))

(defmethod hachee.kkc.convert:convert-extended-dictionary
    ((c kkc-convert))
  (kkc-convert-extended-dictionary c))
