;; hachee.language-model based kkc
(defpackage :hachee.kkc.impl.lm
  (:use :cl)
  (:export :kkc
           :kkc-word-dictionary
           :save-kkc
           :load-kkc
           :build-kkc
           :build-kkc-simple

           :kkc-convert
           :make-kkc-convert))
(in-package :hachee.kkc.impl.lm)

(defstruct kkc
  n-gram-model
  vocabulary
  word-dictionary
  char-dictionary
  unknown-word-vocabulary
  unknown-word-n-gram-model)

(defun build-kkc-simple (pathnames &key char-dictionary)
  (let ((vocabulary (hachee.kkc.impl.lm.build:build-vocabulary pathnames))
        (n-gram-model (make-instance 'hachee.language-model.n-gram:model)))
    (hachee.kkc.impl.lm.build:train-n-gram-model
     n-gram-model pathnames vocabulary)
    (make-kkc
     :n-gram-model n-gram-model
     :vocabulary vocabulary
     :word-dictionary
     (hachee.kkc.impl.lm.build:build-word-dictionary pathnames vocabulary)
     :char-dictionary (or char-dictionary
                          (hachee.kkc.impl.lm.dictionary:make-dictionary))
     :unknown-word-vocabulary
     (hachee.language-model.vocabulary:make-vocabulary)
     :unknown-word-n-gram-model
     (make-instance 'hachee.language-model.n-gram:model))))

(defun build-kkc (pathnames-segmented
                  &key pathnames-inaccurately-segmented
                       word-dictionary-pathnames
                       char-dictionary
                       trusted-word-dictionary
                       class-token-to-word-file-path
                       weights)
  (let ((vocabulary (hachee.kkc.impl.lm.build:build-vocabulary-with-unk
                     pathnames-segmented)))
    (when (and pathnames-inaccurately-segmented
               trusted-word-dictionary
               ;; Unable to map an added word to a class
               (not class-token-to-word-file-path))
      (hachee.kkc.impl.lm.build:extend-existing-vocabulary
       vocabulary
       trusted-word-dictionary
       pathnames-inaccurately-segmented))
    (let ((pathnames
           (append pathnames-segmented
                   pathnames-inaccurately-segmented))
          (n-gram-model
           (if class-token-to-word-file-path
               (make-instance 'hachee.language-model.n-gram:class-model
                              :classifier
                              (hachee.kkc.impl.lm.build:build-classifier
                               class-token-to-word-file-path
                               vocabulary)
                              :weights weights)
               (make-instance 'hachee.language-model.n-gram:model
                              :weights weights))))
      (hachee.kkc.impl.lm.build:train-n-gram-model
       n-gram-model pathnames vocabulary)
      (let* ((word-dictionary
              (hachee.kkc.impl.lm.build:add-to-word-dictionary-from-resources
               (hachee.kkc.impl.lm.build:build-word-dictionary
                pathnames vocabulary)
               word-dictionary-pathnames))
             (unknown-word-vocabulary
              (hachee.kkc.impl.lm.build:build-unknown-word-vocabulary
               pathnames vocabulary))
             (unknown-word-n-gram-model
              (hachee.kkc.impl.lm.build:build-unknown-word-n-gram-model
               pathnames
               vocabulary
               unknown-word-vocabulary)))
        (make-kkc
         :n-gram-model n-gram-model
         :vocabulary vocabulary
         :word-dictionary word-dictionary
         :char-dictionary
         (or char-dictionary
             (hachee.kkc.impl.lm.dictionary:make-dictionary))
         :unknown-word-vocabulary unknown-word-vocabulary
         :unknown-word-n-gram-model unknown-word-n-gram-model)))))

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

(defstruct score-calc-dto
  n-gram-model
  unknown-word-vocabulary
  unknown-word-n-gram-model)

(defun string->sentence (str unknown-word-char-vocabulary)
  (hachee.language-model:make-sentence
   :tokens (loop for ch across str
                 for unit = (hachee.kkc.impl.lm.unit:make-unit
                             :form (string ch)
                             :pron (string ch))
                 for token = (hachee.language-model.vocabulary:to-int-or-unk
                              unknown-word-char-vocabulary
                              (hachee.kkc.impl.lm.unit:unit->key unit))
                 collect token)))

(defun unknown-word-log-probability (score-calc-dto form)
  (with-accessors ((unknown-word-vocabulary
                    score-calc-dto-unknown-word-vocabulary)
                   (unknown-word-n-gram-model
                    score-calc-dto-unknown-word-n-gram-model)) score-calc-dto
    (let ((sentence (string->sentence form unknown-word-vocabulary))
          (bos (hachee.language-model.vocabulary:to-int
                unknown-word-vocabulary
                hachee.language-model.vocabulary:+BOS+))
          (eos (hachee.language-model.vocabulary:to-int
                unknown-word-vocabulary
                 hachee.language-model.vocabulary:+EOS+)))
      (hachee.language-model.n-gram:sentence-log-probability
       unknown-word-n-gram-model sentence :BOS bos :EOS eos))))

(defun transit-probability (score-calc-dto curr-entry prev-entry)
  (let ((curr-token (convert-entry-token curr-entry))
        (prev-token (convert-entry-token prev-entry))
        (n-gram-model (score-calc-dto-n-gram-model score-calc-dto)))
    (hachee.language-model.n-gram:transition-probability
     n-gram-model curr-token (list prev-token))))

(defun compute-convert-score (score-calc-dto curr-entry prev-entry)
  (let ((prob-transit (transit-probability
                       score-calc-dto curr-entry prev-entry)))
    (if (= prob-transit 0)
        ;; The n-gram model was not able to predict the current token
        ;; For example, if the current token is unknown, and the model
        ;; can't predict unknown tokens, the probability will be 0.
        (- #xFFFF)
        (let ((log-prob-unk
               (if (from-vocabulary-p curr-entry)
                   0
                   (unknown-word-log-probability
                    score-calc-dto
                    (hachee.kkc.convert:entry-form curr-entry)))))
          (+ (log prob-transit) log-prob-unk)))))


(defstruct list-dto dictionaries vocabulary)

(defun list-entries (list-dto sub-pron)
  (let ((entries nil))
    (with-accessors ((vocabulary list-dto-vocabulary)
                     (dictionaries list-dto-dictionaries)) list-dto
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
                    entries))))))
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

(defmethod hachee.kkc.convert:convert-score-fn ((kkc kkc))
  (let ((score-calc-dto (make-score-calc-dto
                         :n-gram-model
                         (kkc-n-gram-model kkc)
                         :unknown-word-vocabulary
                         (kkc-unknown-word-vocabulary kkc)
                         :unknown-word-n-gram-model
                         (kkc-unknown-word-n-gram-model kkc))))
    (lambda (curr-entry prev-entry)
      (compute-convert-score score-calc-dto curr-entry prev-entry))))

(defmethod hachee.kkc.convert:convert-list-entries-fn ((kkc kkc))
  (let ((list-dto (make-list-dto
                   :vocabulary (kkc-vocabulary kkc)
                   :dictionaries (list (kkc-word-dictionary kkc)))))
    (lambda (pron)
      (list-entries list-dto pron))))

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
          (score-calc-dto (make-score-calc-dto
                           :n-gram-model
                           (kkc-n-gram-model kkc)
                           :unknown-word-vocabulary
                           (kkc-unknown-word-vocabulary kkc)
                           :unknown-word-n-gram-model
                           (kkc-unknown-word-n-gram-model kkc)))
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
                        score-calc-dto curr-entry prev-entry)
                       (compute-convert-score
                        score-calc-dto next-entry curr-entry)))))))))

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
  (kkc-n-gram-model kkc))

(defmethod hachee.kkc.impl.lm.dump:kkc-unk-vocabulary ((kkc kkc))
  (kkc-unknown-word-vocabulary kkc))

(defmethod hachee.kkc.impl.lm.dump:kkc-unk-model ((kkc kkc))
  (kkc-unknown-word-n-gram-model kkc))
