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
  unit token origin
  unk-log-probability)

(defun make-unknown-word-unit (pron)
  (hachee.kkc.impl.lm.unit:make-unit
   :pron pron
   :form (hachee.ja:hiragana->katakana pron)))

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

(defun string-log-probability (string vocabulary ngram-model)
  (hachee.language-model.ngram:sentence-log-probability
   ngram-model
   (string->sentence string vocabulary)
   :BOS (hachee.language-model.vocabulary:to-int
         vocabulary
         hachee.language-model.vocabulary:+BOS+)
   :EOS (hachee.language-model.vocabulary:to-int
         vocabulary
         hachee.language-model.vocabulary:+EOS+)))

(defun transition-probability (ngram-model curr-entry history-entry-list)
  (let ((token (convert-entry-token curr-entry))
        (history-tokens (mapcar #'convert-entry-token history-entry-list)))
    (hachee.language-model.ngram:model-probability
     ngram-model token history-tokens)))

(defun compute-convert-score (ngram-model
                              curr-entry &rest history-entry-list)
  (let ((transition-prob (transition-probability
                          ngram-model curr-entry history-entry-list)))
    (if (< 0 transition-prob)
        (+ (log transition-prob)
           (convert-entry-unk-log-probability curr-entry))
        ;; The ngram model was not able to predict the current token
        ;; For example, if the current token is unknown, and the model
        ;; can't predict unknown tokens, the probability will be 0.
        (- #xFFFF))))

(defstruct convert-entry-factory
  vocabulary
  unknown-word-vocabulary
  unknown-word-ngram-model)

(defun create-convert-entry (factory unit origin)
  (make-convert-entry
   :unit unit :origin origin
   :token (hachee.language-model.vocabulary:to-int-or-unk
           (convert-entry-factory-vocabulary factory)
           (if (eql origin hachee.kkc.origin:+vocabulary+)
               (hachee.kkc.impl.lm.unit:unit->key unit)
               hachee.language-model.vocabulary:+UNK+))
   :unk-log-probability
   (if (eql origin hachee.kkc.origin:+vocabulary+)
       0
       (string-log-probability
        (hachee.kkc.impl.lm.unit:unit-form unit)
        (convert-entry-factory-unknown-word-vocabulary factory)
        (convert-entry-factory-unknown-word-ngram-model factory)))))

(defun list-entries (sub-pron dictionaries convert-entry-factory)
  (let ((entries nil))
    ;; Add entries from dictionaries
    (dolist (dict dictionaries)
      (dolist (dict-entry (hachee.kkc.impl.lm.dictionary:lookup
                           dict sub-pron))
        (push (create-convert-entry
               convert-entry-factory
               (hachee.kkc.impl.lm.dictionary:entry-unit dict-entry)
               (hachee.kkc.impl.lm.dictionary:entry-origin dict-entry))
              entries)))
    ;; Add unknown word entry if necessary
    (when (< (length sub-pron) 8) ;; Length up to 8
      (let ((unk-unit (make-unknown-word-unit sub-pron)))
        (when (not (some (lambda (dict)
                           (hachee.kkc.impl.lm.dictionary:contains-p
                            dict
                            unk-unit))
                         dictionaries))
          (push (create-convert-entry
                 convert-entry-factory
                 unk-unit
                 hachee.kkc.origin:+out-of-dictionary+)
                entries))))
    (nreverse entries)))

(defun kkc-convert-entry-factory (kkc)
  (make-convert-entry-factory
   :vocabulary (kkc-vocabulary kkc)
   :unknown-word-vocabulary (kkc-unknown-word-vocabulary kkc)
   :unknown-word-ngram-model (kkc-unknown-word-ngram-model kkc)))

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
   :origin hachee.kkc.origin:+vocabulary+
   :unk-log-probability 0))

(defmethod hachee.kkc.convert:convert-list-entries-fn ((kkc kkc))
  (let ((factory (kkc-convert-entry-factory kkc))
        (dictionaries (list (kkc-word-dictionary kkc))))
    (lambda (pron)
      (list-entries pron dictionaries factory))))

(defmethod hachee.kkc.convert:convert-score-fn ((kkc kkc-2gram))
  (let ((ngram-model (kkc-ngram-model kkc)))
    (lambda (curr-entry prev-entry)
      (compute-convert-score ngram-model curr-entry prev-entry))))

(defmethod hachee.kkc.convert:convert-score-fn ((kkc kkc-3gram))
  (let ((ngram-model (kkc-ngram-model kkc)))
    (lambda (curr-entry prev2-entry prev1-entry)
      (compute-convert-score ngram-model
                             curr-entry prev2-entry prev1-entry))))

;;; lookup

(defun lookup (pronunciation
               &key score-fn word-dict char-dict convert-entry-factory)
  (let ((result-items nil))
    (labels ((push-items (dictionary-entries)
               (dolist (dictionary-entry dictionary-entries)
                 (let ((item (create-convert-entry
                              convert-entry-factory
                              (hachee.kkc.impl.lm.dictionary:entry-unit
                               dictionary-entry)
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

(defun lookup-score-fn (ngram-model prev-entry next-entry)
  (labels ((score (curr-entry)
             (+ (compute-convert-score
                 ngram-model curr-entry prev-entry)
                (compute-convert-score
                 ngram-model next-entry curr-entry))))
    (let ((score-cache (make-hash-table :test #'equal)))
      (lambda (curr-entry)
        (let ((key (hachee.kkc.impl.lm.unit:unit->key
                    (convert-entry-unit curr-entry))))
          (or (gethash key score-cache)
              (setf (gethash key score-cache) (score curr-entry))))))))

(defmethod hachee.kkc.lookup:execute ((kkc kkc) (pronunciation string)
                                      &key prev next)
  (let ((factory (kkc-convert-entry-factory kkc)))
    (lookup pronunciation
     :score-fn
     (when (and next prev)
       (let ((prev-entry
              (create-convert-entry
               factory prev hachee.kkc.origin:+runtime-none+))
             (next-entry
              (create-convert-entry
               factory next hachee.kkc.origin:+runtime-none+)))
         (lookup-score-fn (kkc-ngram-model kkc) prev-entry next-entry)))
     :word-dict (kkc-word-dictionary kkc)
     :char-dict (kkc-char-dictionary kkc)
     :convert-entry-factory factory)))

(defmethod hachee.kkc.lookup:item-form ((item convert-entry))
  (hachee.kkc.impl.lm.unit:unit-form (convert-entry-unit item)))

(defmethod hachee.kkc.lookup:item-origin ((item convert-entry))
  (convert-entry-origin item))

;;; dump

(defmethod hachee.kkc.impl.lm.dump:kkc-class-vocabulary ((kkc kkc))
  (kkc-vocabulary kkc))

(defmethod hachee.kkc.impl.lm.dump:kkc-class-model ((kkc kkc))
  (kkc-ngram-model kkc))

(defmethod hachee.kkc.impl.lm.dump:kkc-unk-vocabulary ((kkc kkc))
  (kkc-unknown-word-vocabulary kkc))

(defmethod hachee.kkc.impl.lm.dump:kkc-unk-model ((kkc kkc))
  (kkc-unknown-word-ngram-model kkc))
