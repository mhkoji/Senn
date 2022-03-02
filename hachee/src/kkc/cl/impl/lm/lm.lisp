;; hachee.language-model based kkc
(defpackage :hachee.kkc.impl.lm
  (:use :cl)
  (:export :kkc
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
  sum-probabilities-of-vocabulary-words
  unknown-word-vocabulary
  unknown-word-n-gram-model)

(defstruct kkc-convert
  kkc
  extended-dictionary)

(defun save-kkc (kkc pathname)
  (hachee.kkc.impl.lm.persist:do-save-into-zip (add-entry pathname)
    (add-entry "n-gram-model.txt"
               (kkc-n-gram-model kkc)
               (if (typep (kkc-n-gram-model kkc)
                          'hachee.language-model.n-gram:class-model)
                   'hachee.language-model.n-gram:class-model
                   'hachee.language-model.n-gram:model))
    (add-entry "vocabulary.txt"
               (kkc-vocabulary kkc))
    (add-entry "word-dictionary.txt"
               (kkc-word-dictionary kkc))
    (add-entry "char-dictionary.txt"
               (kkc-char-dictionary kkc))
    (add-entry "unknown-word-vocabulary.txt"
               (kkc-unknown-word-vocabulary kkc))
    (add-entry "unknown-word-n-gram-model.txt"
               (kkc-unknown-word-n-gram-model kkc)))
  (values))


(defun pron->sentence (pron unknown-word-char-vocabulary)
  (hachee.language-model:make-sentence
   :tokens (loop for ch across pron
                 for unit = (hachee.kkc.impl.lm.unit:make-unit
                             :form (string ch)
                             :pron (string ch))
                 collect (hachee.language-model.vocabulary::to-int-or-unk
                          unknown-word-char-vocabulary
                          (hachee.kkc.impl.lm.unit:unit->key unit)))))

(defun sum-probabilities-of-words (unknown-word-vocabulary
                                   unknown-word-n-gram-model
                                   words)
  (let ((bos-token (hachee.language-model.vocabulary:to-int
                    unknown-word-vocabulary
                    hachee.language-model.vocabulary:+BOS+))
        (eos-token (hachee.language-model.vocabulary:to-int
                    unknown-word-vocabulary
                    hachee.language-model.vocabulary:+EOS+)))
    (loop
      for word in words
      for sent = (pron->sentence (hachee.kkc.impl.lm.unit:unit-pron word)
                                 unknown-word-vocabulary)
      sum (exp (hachee.language-model.n-gram:sentence-log-probability
                unknown-word-n-gram-model sent
                :BOS bos-token
                :EOS eos-token)))))

(defun sum-probabilities-of-vocabulary-words (unknown-word-vocabulary
                                              unknown-word-n-gram-model
                                              word-dictionary)
  (sum-probabilities-of-words
   unknown-word-vocabulary
   unknown-word-n-gram-model
   (mapcar #'hachee.kkc.impl.lm.dictionary:entry-unit
           (remove-if-not
            (lambda (e)
              (eql (hachee.kkc.impl.lm.dictionary:entry-origin e)
                   hachee.kkc.origin:+vocabulary+))
            (hachee.kkc.impl.lm.dictionary:list-all word-dictionary)))))

(defun load-kkc (pathname)
  (let ((entry-alist (hachee.kkc.impl.lm.persist:load-from-zip pathname)))
    (labels ((ensure-not-null (x)
               (assert x)
               x)
             (get-entry (filename)
               (ensure-not-null
                (cdr (assoc filename entry-alist :test #'string=)))))
      (let ((unknown-word-vocabulary
             (get-entry "unknown-word-vocabulary.txt"))
            (unknown-word-n-gram-model
             (get-entry "unknown-word-n-gram-model.txt"))
            (word-dictionary
             (get-entry "word-dictionary.txt")))
        (make-kkc
         :n-gram-model
         (get-entry "n-gram-model.txt")
         :vocabulary
         (get-entry "vocabulary.txt")
         :word-dictionary word-dictionary
         :char-dictionary
         (get-entry "char-dictionary.txt")
         :unknown-word-vocabulary unknown-word-vocabulary
         :unknown-word-n-gram-model unknown-word-n-gram-model
         :sum-probabilities-of-vocabulary-words
         (sum-probabilities-of-vocabulary-words unknown-word-vocabulary
                                                unknown-word-n-gram-model
                                                word-dictionary))))))

(defun build-kkc-simple (pathnames &key char-dictionary)
  (let ((vocabulary (hachee.kkc.impl.lm.build:build-vocabulary pathnames))
        (n-gram-model (make-instance 'hachee.language-model.n-gram:model)))
    (hachee.kkc.impl.lm.build:train-n-gram-model n-gram-model pathnames vocabulary)
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
     (make-instance 'hachee.language-model.n-gram:model)
     :sum-probabilities-of-vocabulary-words 0)))

(defun build-kkc (pathnames-segmented
                  &key pathnames-inaccurately-segmented
                       word-dictionary-pathnames
                       char-dictionary
                       trusted-word-dictionary
                       class-token-to-word-file-path)
  (let ((vocabulary
         (hachee.kkc.impl.lm.build:build-vocabulary-with-unk pathnames-segmented)))
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
                              :weights (list 0.115267 0.884733))
               (make-instance 'hachee.language-model.n-gram:model
                              :weights (list 0.253401 0.746599)))))
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
         :unknown-word-n-gram-model unknown-word-n-gram-model
         :sum-probabilities-of-vocabulary-words
         (sum-probabilities-of-vocabulary-words unknown-word-vocabulary
                                                unknown-word-n-gram-model
                                                word-dictionary))))))

;;; convert

(defvar *empty-dictionary*
  (hachee.kkc.impl.lm.dictionary:make-dictionary))

(defstruct convert-entry
  unit token origin)

(defun from-vocabulary-p (entry)
  (eql (convert-entry-origin entry)
       hachee.kkc.origin:+vocabulary+))

(defun from-extended-dictionary-p (entry)
  (eql (convert-entry-origin entry)
       hachee.kkc.origin:+extended-dictionary+))

(defun make-unknown-word-unit (pron)
  (hachee.kkc.impl.lm.unit:make-unit
   :pron pron
   :form (hachee.ja:hiragana->katakana pron)))

(defstruct score-calc-dto
  n-gram-model
  unknown-word-vocabulary
  unknown-word-n-gram-model
  sum-probabilities-of-vocabulary-words
  extended-dictionary)

(defun unknown-word-log-probability (score-calc-dto pron)
  (with-accessors ((unknown-word-vocabulary
                    score-calc-dto-unknown-word-vocabulary)
                   (unknown-word-n-gram-model
                    score-calc-dto-unknown-word-n-gram-model)) score-calc-dto
    (let ((sentence (pron->sentence pron unknown-word-vocabulary))
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
           (hachee.kkc.impl.lm.dictionary:size extended-dictionary)))
      (if (and (from-extended-dictionary-p entry)
               (< 0 extended-dictionary-size)
               (< 0 sum-probabilities-of-vocabulary-words))
          (/ sum-probabilities-of-vocabulary-words
             extended-dictionary-size)
          0))))

(defun transit-probability (score-calc-dto curr-entry prev-entry)
  (let ((curr-token (convert-entry-token curr-entry))
        (prev-token (convert-entry-token prev-entry))
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
              (let ((log-prob-unknown
                     (unknown-word-log-probability
                      score-calc-dto
                      (hachee.kkc.convert:entry-pron curr-entry)))
                    (prob-extended
                     (extended-dictionary-word-probability
                      score-calc-dto
                      curr-entry)))
                (if (< 0 prob-extended)
                    (log (+ (exp log-prob-unknown)
                            prob-extended))
                    log-prob-unknown)))))))

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
                           (kkc-unknown-word-n-gram-model kkc)
                           :sum-probabilities-of-vocabulary-words
                           (kkc-sum-probabilities-of-vocabulary-words kkc)
                           :extended-dictionary *empty-dictionary*))
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
