;; hachee.language-model based kkc
(defpackage :hachee.kkc.impl.lm
  (:use :cl)
  (:export :kkc
           :kkc-2gram
           :kkc-3gram
           :kkc-word-dictionary))
(in-package :hachee.kkc.impl.lm)

(defclass kkc ()
  ((ngram-model
    :initarg :ngram-model
    :reader kkc-ngram-model)
   (vocabulary
    :initarg :vocabulary
    :reader kkc-vocabulary)
   (word-dictionary
    :initarg :word-dictionary
    :reader kkc-word-dictionary)
   (char-dictionary
    :initarg :char-dictionary
    :reader kkc-char-dictionary)
   (unknown-word-vocabulary
    :initarg :unknown-word-vocabulary
    :reader kkc-unknown-word-vocabulary)
   (unknown-word-ngram-model
    :initarg :unknown-word-ngram-model
    :reader kkc-unknown-word-ngram-model)))

(defclass kkc-2gram (kkc hachee.kkc.convert:2gram-convert)
  ())

(defclass kkc-3gram (kkc hachee.kkc.convert:2gram-convert)
  ())

;;; convert

(defstruct convert-entry
  unit token origin)

(defun from-vocabulary-p (entry)
  (eql (convert-entry-origin entry)
       hachee.kkc.origin:+vocabulary+))

(defun make-unknown-word-unit (pron)
  (hachee.kkc.impl.lm.unit:make-unit
   :pron pron
   :form (hachee.ja:hiragana->katakana pron)))

(defstruct score-calculator
  ngram-model
  unknown-word-vocabulary
  unknown-word-ngram-model)

(defun string->sentence (str unknown-word-char-vocabulary)
  (hachee.language-model.ngram:make-sentence
   :tokens (loop for ch across str
                 for unit = (hachee.kkc.impl.lm.unit:make-unit
                             :form (string ch)
                             :pron (string ch))
                 for token = (hachee.language-model.vocabulary:to-int-or-unk
                              unknown-word-char-vocabulary
                              (hachee.kkc.impl.lm.unit:unit->key unit))
                 collect token)))

(defun unknown-word-log-probability (score-calculator form)
  (let ((unknown-word-vocabulary
         (score-calculator-unknown-word-vocabulary score-calculator))
        (unknown-word-ngram-model
         (score-calculator-unknown-word-ngram-model score-calculator)))
    (let ((sentence (string->sentence form unknown-word-vocabulary))
          (bos (hachee.language-model.vocabulary:to-int
                unknown-word-vocabulary
                hachee.language-model.vocabulary:+BOS+))
          (eos (hachee.language-model.vocabulary:to-int
                unknown-word-vocabulary
                hachee.language-model.vocabulary:+EOS+)))
      (hachee.language-model.ngram:sentence-log-probability
       unknown-word-ngram-model sentence :BOS bos :EOS eos))))

(defun transition-probability (score-calculator curr-entry history-entry-list)
  (let ((model (score-calculator-ngram-model score-calculator))
        (token (convert-entry-token curr-entry))
        (history-tokens (mapcar #'convert-entry-token history-entry-list)))
    (hachee.language-model.ngram:model-probability
     model token history-tokens)))

(defun convert-entry-unk-log-probability (score-calculator entry)
  (if (from-vocabulary-p entry)
      0
      (let ((form (hachee.kkc.convert:entry-form entry)))
        (unknown-word-log-probability score-calculator form))))

(defun compute-convert-score (score-calculator curr-entry
                              &rest history-entry-list)
  (let ((transition-prob (transition-probability
                          score-calculator curr-entry history-entry-list)))
    (if (< 0 transition-prob)
        (+ (log transition-prob)
           (convert-entry-unk-log-probability score-calculator curr-entry))
        ;; The ngram model was not able to predict the current token
        ;; For example, if the current token is unknown, and the model
        ;; can't predict unknown tokens, the probability will be 0.
        (- #xFFFF))))

(defun list-entries (sub-pron dictionaries vocabulary)
  (let ((entries nil))
    ;; Add entries from dictionaries
    (dolist (dict dictionaries)
      (dolist (dict-entry (hachee.kkc.impl.lm.dictionary:lookup
                           dict sub-pron))
        (let* ((unit (hachee.kkc.impl.lm.dictionary:entry-unit dict-entry))
               (token (hachee.language-model.vocabulary:to-int-or-unk
                       vocabulary
                       (hachee.kkc.impl.lm.unit:unit->key unit)))
               (origin (hachee.kkc.impl.lm.dictionary:entry-origin
                        dict-entry)))
          (push (make-convert-entry
                 :unit unit :token token :origin origin)
                entries))))
    ;; Add unknown word entry if necessary
    (when (< (length sub-pron) 8) ;; Length up to 8
      (let ((unk-unit (make-unknown-word-unit sub-pron)))
        (when (not (some (lambda (dict)
                           (hachee.kkc.impl.lm.dictionary:contains-p
                            dict
                            unk-unit))
                         dictionaries))
          (let ((token (hachee.language-model.vocabulary:to-int
                        vocabulary
                        hachee.language-model.vocabulary:+UNK+))
                (origin hachee.kkc.origin:+out-of-dictionary+))
            (push (make-convert-entry
                   :unit unk-unit :token token :origin origin)
                  entries)))))
    (nreverse entries)))

(defmethod hachee.kkc.convert:entry-form ((e convert-entry))
  (hachee.kkc.impl.lm.unit:unit-form (convert-entry-unit e)))

(defmethod hachee.kkc.convert:entry-pron ((e convert-entry))
  (hachee.kkc.impl.lm.unit:unit-pron (convert-entry-unit e)))

(defmethod hachee.kkc.convert:entry-origin ((e convert-entry))
  (convert-entry-origin e))

(defmethod hachee.kkc.convert:convert-begin-entry ((kkc kkc))
  (make-convert-entry
   :token (hachee.language-model.vocabulary:to-int
           (kkc-vocabulary kkc)
           hachee.language-model.vocabulary:+BOS+)
   :origin hachee.kkc.origin:+vocabulary+))

(defmethod hachee.kkc.convert:convert-end-entry ((kkc kkc))
  (make-convert-entry
   :token (hachee.language-model.vocabulary:to-int
           (kkc-vocabulary kkc)
           hachee.language-model.vocabulary:+EOS+)
   :origin hachee.kkc.origin:+vocabulary+))

(defmethod hachee.kkc.convert:convert-list-entries-fn ((kkc kkc))
  (let ((vocabulary (kkc-vocabulary kkc))
        (dictionaries (list (kkc-word-dictionary kkc))))
    (lambda (pron)
      (list-entries pron dictionaries vocabulary))))


(defmethod hachee.kkc.convert:convert-score-fn ((kkc kkc-2gram))
  (let ((score-calculator (make-score-calculator
                           :ngram-model
                           (kkc-ngram-model kkc)
                           :unknown-word-vocabulary
                           (kkc-unknown-word-vocabulary kkc)
                           :unknown-word-ngram-model
                           (kkc-unknown-word-ngram-model kkc))))
    (lambda (curr-entry prev-entry)
      (compute-convert-score score-calculator curr-entry prev-entry))))

(defmethod hachee.kkc.convert:convert-score-fn ((kkc kkc-3gram))
  (let ((score-calculator (make-score-calculator
                           :ngram-model
                           (kkc-ngram-model kkc)
                           :unknown-word-vocabulary
                           (kkc-unknown-word-vocabulary kkc)
                           :unknown-word-ngram-model
                           (kkc-unknown-word-ngram-model kkc))))
    (lambda (curr-entry prev2-entry prev1-entry)
      (compute-convert-score score-calculator
                             curr-entry prev2-entry prev1-entry))))

;;; lookup

(defstruct lookup-item unit origin)

(defun lookup (pronunciation &key score-fn word-dict char-dict)
  (let ((result-items nil))
    (labels ((push-items (dictionary-entries)
               (dolist (dictionary-entry dictionary-entries)
                 (let ((item (make-lookup-item
                              :unit
                              (hachee.kkc.impl.lm.dictionary:entry-unit
                               dictionary-entry)
                              :origin
                              (hachee.kkc.impl.lm.dictionary:entry-origin
                               dictionary-entry))))
                   (pushnew item result-items
                            :test #'string=
                            :key #'hachee.kkc.lookup:item-form)))))
      (push-items (hachee.kkc.impl.lm.dictionary:lookup
                   word-dict
                   pronunciation))
      (when score-fn
        (setq result-items (sort result-items #'< :key score-fn)))
      (push-items (hachee.kkc.impl.lm.dictionary:lookup
                   char-dict
                   pronunciation)))
    ;; ch_1, ..., ch_s, w_1, ..., w_t,
    ;;     where score(w_1) < score(w_2) < ... < score(w_t)
    ;; => w_t, ..., w_1, ch_s, ..., ch_1
    ;; We don't care about the order of the chars ch_1, ..., ch_s.
    (nreverse result-items)))

(defun lookup-score-fn (kkc prev-unit next-unit)
  (labels ((unit->convert-entry (unit origin)
             (make-convert-entry
              :unit unit
              :token (hachee.language-model.vocabulary:to-int-or-unk
                      (kkc-vocabulary kkc)
                      (hachee.kkc.impl.lm.unit:unit->key unit))
              :origin origin)))
    (let ((prev-entry (unit->convert-entry
                       prev-unit
                       hachee.kkc.origin:+runtime-none+))
          (next-entry (unit->convert-entry
                       next-unit
                       hachee.kkc.origin:+runtime-none+))
          (score-calculator (make-score-calculator
                             :ngram-model
                             (kkc-ngram-model kkc)
                             :unknown-word-vocabulary
                             (kkc-unknown-word-vocabulary kkc)
                             :unknown-word-ngram-model
                             (kkc-unknown-word-ngram-model kkc)))
          (score-cache (make-hash-table :test #'equal)))
      (lambda (curr-item)
        (let ((curr-entry (unit->convert-entry
                           (lookup-item-unit curr-item)
                           (lookup-item-origin curr-item)))
              (key (hachee.kkc.impl.lm.unit:unit->key
                    (lookup-item-unit curr-item))))
          (or (gethash key score-cache)
              (setf (gethash key score-cache)
                    (+ (compute-convert-score
                        score-calculator curr-entry prev-entry)
                       (compute-convert-score
                        score-calculator next-entry curr-entry)))))))))

(defmethod hachee.kkc.lookup:execute ((kkc kkc) (pronunciation string)
                                      &key prev next)
  (lookup pronunciation
   :score-fn (when (and next prev)
               (lookup-score-fn kkc prev next))
   :word-dict (kkc-word-dictionary kkc)
   :char-dict (kkc-char-dictionary kkc)))

(defmethod hachee.kkc.lookup:item-form ((item lookup-item))
  (hachee.kkc.impl.lm.unit:unit-form (lookup-item-unit item)))

(defmethod hachee.kkc.lookup:item-origin ((item lookup-item))
  (lookup-item-origin item))

;;; dump

(defmethod hachee.kkc.impl.lm.dump:kkc-class-vocabulary ((kkc kkc))
  (kkc-vocabulary kkc))

(defmethod hachee.kkc.impl.lm.dump:kkc-class-model ((kkc kkc))
  (kkc-ngram-model kkc))

(defmethod hachee.kkc.impl.lm.dump:kkc-unk-vocabulary ((kkc kkc))
  (kkc-unknown-word-vocabulary kkc))

(defmethod hachee.kkc.impl.lm.dump:kkc-unk-model ((kkc kkc))
  (kkc-unknown-word-ngram-model kkc))
