(defpackage :hachee.kkc
  (:use :cl)
  (:export :kkc
           :kkc-extended-dictionary
           :save-kkc
           :load-kkc
           :build-kkc
           :build-kkc-simple
           :convert
           :lookup
           :profile))
(in-package :hachee.kkc)

(defun from-vocabulary-p (entry)
  (eql (hachee.kkc.convert:entry-origin entry)
       hachee.kkc.origin:+vocabulary+))

(defun from-extended-dictionary-p (entry)
  (eql (hachee.kkc.convert:entry-origin entry)
       hachee.kkc.origin:+extended-dictionary+))


;; word-pron pair n-gram model
(defstruct kkc
  n-gram-model
  vocabulary
  word-dictionary
  char-dictionary
  extended-dictionary

  sum-probabilities-of-vocabulary-words
  unknown-word-vocabulary
  unknown-word-n-gram-model)

(defun save-kkc (kkc pathname)
  (zip:with-output-to-zipfile (writer pathname)
    (labels ((add-file (name data-string)
               (flexi-streams:with-input-from-sequence
                   (data-stream (flexi-streams:string-to-octets
                                 data-string
                                 :external-format :utf-8))
                 (zip:write-zipentry writer name data-stream
                                     :file-write-date
                                     (get-universal-time)))))
      (let ((n-gram-model (kkc-n-gram-model kkc)))
        (add-file (if (typep n-gram-model
                             'hachee.language-model.n-gram:class-model)
                      "class-n-gram-model.txt"
                      "n-gram-model.txt")
                  (with-output-to-string (stream)
                    (hachee.language-model.n-gram:save-model n-gram-model
                                                             stream))))
      (loop for (filename object)
            in (list (list "vocabulary.txt"
                           (kkc-vocabulary kkc))
                     (list "word-dictionary.txt"
                           (kkc-word-dictionary kkc))
                     (list "char-dictionary.txt"
                           (kkc-char-dictionary kkc))
                     (list "extended-dictionary.txt"
                           (kkc-extended-dictionary kkc))
                     (list "unknown-word-vocabulary.txt"
                           (kkc-unknown-word-vocabulary kkc))
                     (list "unknown-word-n-gram-model.txt"
                           (kkc-unknown-word-n-gram-model kkc)))
        for data-string = (with-output-to-string (s)
                            (hachee.kkc.archive:save-object object s))
        do (progn
              (add-file filename data-string)))))
  (values))

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
      sum (exp (hachee.language-model.n-gram:sentence-log-probability
                unknown-word-n-gram-model
                (hachee.kkc.util:unit->sentence word unknown-word-vocabulary)
                :BOS bos-token
                :EOS eos-token)))))

(defun sum-probabilities-of-vocabulary-words (unknown-word-vocabulary
                                              unknown-word-n-gram-model
                                              word-dictionary)
  (sum-probabilities-of-words
   unknown-word-vocabulary
   unknown-word-n-gram-model
   (mapcar #'hachee.kkc.dictionary:entry-unit
           (remove-if-not (lambda (e)
                            (eql (hachee.kkc.dictionary:entry-origin e)
                                 hachee.kkc.origin:+vocabulary+))
                          (hachee.kkc.dictionary:list-all word-dictionary)))))

(defun load-kkc (pathname)
  (zip:with-zipfile (zip pathname)
    (labels ((read-from-file (filename read-from-stream-fn)
               (let ((entry (zip:get-zipfile-entry filename zip)))
                 (when entry
                   (let ((octets (zip:zipfile-entry-contents entry)))
                     (let ((string (babel:octets-to-string
                                    octets
                                    :encoding :utf-8)))
                       (with-input-from-string (s string)
                         (funcall read-from-stream-fn s)))))))
             (load-from-file (type filename)
               (read-from-file filename
                               (lambda (s)
                                 (hachee.kkc.archive:load-object-as type s))))
             (ensure-not-null (x)
               (assert x)
               x))
      (let ((unknown-word-vocabulary
             (ensure-not-null
              (load-from-file 'hachee.language-model.vocabulary:vocabulary
                              "unknown-word-vocabulary.txt")))
            (unknown-word-n-gram-model
             (ensure-not-null
              (load-from-file 'hachee.language-model.n-gram:model
                              "unknown-word-n-gram-model.txt")))
            (word-dictionary
             (ensure-not-null
              (load-from-file 'hachee.kkc.dictionary:dictionary
                              "word-dictionary.txt"))))
        (make-kkc
         :n-gram-model
         (ensure-not-null
          (or (load-from-file 'hachee.language-model.n-gram:class-model
                              "class-n-gram-model.txt")
              (load-from-file 'hachee.language-model.n-gram:model
                              "n-gram-model.txt")))
         :vocabulary
         (ensure-not-null
          (load-from-file 'hachee.language-model.vocabulary:vocabulary
                          "vocabulary.txt"))
         :word-dictionary word-dictionary
         :char-dictionary
         (ensure-not-null
          (load-from-file 'hachee.kkc.dictionary:dictionary
                          "char-dictionary.txt"))
         :extended-dictionary
         (ensure-not-null
          (load-from-file 'hachee.kkc.dictionary:dictionary
                          "extended-dictionary.txt"))
         :unknown-word-vocabulary unknown-word-vocabulary
         :unknown-word-n-gram-model unknown-word-n-gram-model
         :sum-probabilities-of-vocabulary-words
         (sum-probabilities-of-vocabulary-words unknown-word-vocabulary
                                                unknown-word-n-gram-model
                                                word-dictionary))))))

(defun build-kkc-simple (pathnames &key char-dictionary)
  (let ((vocabulary (hachee.kkc.build:build-vocabulary pathnames))
        (n-gram-model (make-instance 'hachee.language-model.n-gram:model)))
    (hachee.kkc.build:train-n-gram-model n-gram-model pathnames vocabulary)
    (make-kkc
     :n-gram-model n-gram-model
     :vocabulary vocabulary
     :word-dictionary
     (hachee.kkc.build:build-word-dictionary pathnames vocabulary)
     :char-dictionary (or char-dictionary
                          (hachee.kkc.dictionary:make-dictionary))
     :extended-dictionary (hachee.kkc.dictionary:make-dictionary)
     :unknown-word-vocabulary
     (hachee.language-model.vocabulary:make-vocabulary)
     :unknown-word-n-gram-model
     (make-instance 'hachee.language-model.n-gram:model)
     :sum-probabilities-of-vocabulary-words 0)))

(defun build-kkc (pathnames-segmented
                  &key pathnames-inaccurately-segmented
                       word-dictionary-pathnames
                       char-dictionary
                       extended-dictionary
                       trusted-word-dictionary
                       class-token-to-word-file-path)
  (let ((vocabulary
         (hachee.kkc.build:build-vocabulary-with-unk pathnames-segmented)))
    (when (and pathnames-inaccurately-segmented
               trusted-word-dictionary
               ;; Unable to map an added word to a class
               (not class-token-to-word-file-path))
      (hachee.kkc.build:extend-existing-vocabulary
       vocabulary
       trusted-word-dictionary
       pathnames-inaccurately-segmented))
    (let ((pathnames
           (append pathnames-segmented
                   pathnames-inaccurately-segmented))
          (n-gram-model
           (if class-token-to-word-file-path
               (make-instance 'hachee.language-model.n-gram:class-model
                              :classifier (hachee.kkc.build:build-classifier
                                           class-token-to-word-file-path
                                           vocabulary)
                              :weights (list 0.115267 0.884733))
               (make-instance 'hachee.language-model.n-gram:model
                              :weights (list 0.253401 0.746599)))))
      (hachee.kkc.build:train-n-gram-model n-gram-model pathnames vocabulary)
      (let* ((word-dictionary
              (hachee.kkc.build:add-to-word-dictionary-from-resources
               (hachee.kkc.build:build-word-dictionary pathnames vocabulary)
               word-dictionary-pathnames))
             (unknown-word-vocabulary
              (hachee.kkc.build:build-unknown-word-vocabulary pathnames
                                                              vocabulary))
             (unknown-word-n-gram-model
              (hachee.kkc.build:build-unknown-word-n-gram-model
               pathnames
               vocabulary
               unknown-word-vocabulary)))
        (make-kkc
         :n-gram-model n-gram-model
         :vocabulary vocabulary
         :word-dictionary word-dictionary
         :char-dictionary (or char-dictionary
                              (hachee.kkc.dictionary:make-dictionary))
         :extended-dictionary (or extended-dictionary
                                  (hachee.kkc.dictionary:make-dictionary))
         :unknown-word-vocabulary unknown-word-vocabulary
         :unknown-word-n-gram-model unknown-word-n-gram-model
         :sum-probabilities-of-vocabulary-words
         (sum-probabilities-of-vocabulary-words unknown-word-vocabulary
                                                unknown-word-n-gram-model
                                                word-dictionary))))))

;;; Convert
(defun make-unknown-word-unit (pron)
  (hachee.kkc.dictionary:make-unit
   :pron pron
   :form (hachee.ja:hiragana->katakana pron)))

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
    (let ((entries (mapcar #'dictionary-entry->convert-entry
                           (alexandria:mappend
                            (lambda (dict)
                              (hachee.kkc.dictionary:lookup dict sub-pron))
                            dictionaries))))
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

(defun unknown-word-log-probability (entry kkc)
  (let ((unknown-word-pron-vocabulary
         (kkc-unknown-word-vocabulary kkc))
        (unknown-word-pron-n-gram-model
         (kkc-unknown-word-n-gram-model kkc))
        (extended-dictionary-size
         (hachee.kkc.dictionary:size (kkc-extended-dictionary kkc))))
    (let ((pron-bos-token (hachee.language-model.vocabulary:to-int
                           unknown-word-pron-vocabulary
                           hachee.language-model.vocabulary:+BOS+))
          (pron-eos-token (hachee.language-model.vocabulary:to-int
                           unknown-word-pron-vocabulary
                           hachee.language-model.vocabulary:+EOS+))
          (pron-sentence (hachee.kkc.util:unit->sentence
                          (hachee.kkc.convert:entry-unit entry)
                          unknown-word-pron-vocabulary)))
      (let ((log-prob-by-unknown-word-n-gram
             (hachee.language-model.n-gram:sentence-log-probability
              unknown-word-pron-n-gram-model pron-sentence
              :BOS pron-bos-token
              :EOS pron-eos-token)))
        (if (and (from-extended-dictionary-p entry)
                 (< 0 extended-dictionary-size))
            (let ((probability-for-extended-dictionary-words
                   (/ (kkc-sum-probabilities-of-vocabulary-words kkc)
                      extended-dictionary-size)))
              (log (+ (exp log-prob-by-unknown-word-n-gram)
                      probability-for-extended-dictionary-words)))
            log-prob-by-unknown-word-n-gram)))))

(defun compute-convert-score (kkc curr-entry prev-entry)
  (let ((p (hachee.language-model.n-gram:transition-probability
            (kkc-n-gram-model kkc)
            (hachee.kkc.convert:entry-token curr-entry)
            (list (hachee.kkc.convert:entry-token prev-entry)))))
    (cond ((= p 0)
           -10000)
          ((from-vocabulary-p curr-entry)
           (log p))
          (t
           (+ (log p) (unknown-word-log-probability curr-entry kkc))))))

(defun kkc-convert-begin-entry (kkc)
  (hachee.kkc.convert:make-entry
   :unit hachee.language-model.vocabulary:+BOS+
   :token (hachee.language-model.vocabulary:to-int
           (kkc-vocabulary kkc)
           hachee.language-model.vocabulary:+BOS+)
   :origin hachee.kkc.origin:+vocabulary+))

(defun kkc-convert-end-entry (kkc)
  (hachee.kkc.convert:make-entry
   :unit hachee.language-model.vocabulary:+EOS+
   :token (hachee.language-model.vocabulary:to-int
           (kkc-vocabulary kkc)
           hachee.language-model.vocabulary:+EOS+)
   :origin hachee.kkc.origin:+vocabulary+))

(defun convert (kkc pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi:execute pronunciation
   :begin-entry
   (kkc-convert-begin-entry kkc)
   :end-entry
   (kkc-convert-end-entry kkc)
   :score-fn
   (lambda (curr-entry prev-entry)
     (compute-convert-score kkc curr-entry prev-entry))
   :list-entries-fn
   (lambda (sub-pron)
     (list-entries sub-pron
                   :dictionaries (list (kkc-word-dictionary kkc)
                                       (kkc-extended-dictionary kkc))
                   :vocabulary (kkc-vocabulary kkc)))
   :1st-boundary-index 1st-boundary-index))


;;; Lookup
(defun gen-lookup-score-fn (prev-unit next-unit kkc)
  (labels ((unit->entry (unit)
             (hachee.kkc.convert:make-entry
              :unit unit
              :token (hachee.language-model.vocabulary:to-int-or-unk
                      (kkc-vocabulary kkc)
                      unit)
              :origin hachee.kkc.origin:+runtime-none+)))
    (let ((prev-entry (unit->entry prev-unit))
          (next-entry (unit->entry next-unit))
          (score-cache (make-hash-table :test #'equal)))
      (lambda (curr-item)
        (let ((curr-entry
               (hachee.kkc.convert:make-entry
                :unit (hachee.kkc.lookup:item-unit curr-item)
                :token (hachee.language-model.vocabulary:to-int-or-unk
                        (kkc-vocabulary kkc)
                        (hachee.kkc.lookup:item-unit curr-item))
                :origin (hachee.kkc.lookup:item-origin curr-item))))
          (let ((key (hachee.kkc.dictionary:unit->key
                      (hachee.kkc.lookup:item-unit curr-item))))
            (or (gethash key score-cache)
                (setf (gethash key score-cache)
                      (+ (compute-convert-score
                          curr-entry prev-entry kkc)
                         (compute-convert-score
                          next-entry curr-entry kkc))))))))))

(defun lookup (kkc pronunciation &key prev next)
  (hachee.kkc.lookup:execute pronunciation
   :score-fn (when (and next prev)
               (gen-lookup-score-fn prev next kkc))
   :word-dict (kkc-word-dictionary kkc)
   :char-dict (kkc-char-dictionary kkc)))
