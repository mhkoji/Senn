(defpackage :hachee.kkc
  (:use :cl)
  (:export :+origin-vocabulary+
           :+origin-extended-dictionary+
           :+origin-corpus+
           :+origin-resource+
           :+origin-unknown-word+
           :+origin-tankan+

           :kkc
           :convert
           :get-convert-score-fn
           :lookup
           :get-lookup-score-fn
           :profile))
(in-package :hachee.kkc)

(defparameter +origin-vocabulary+          :vocabulary)
(defparameter +origin-extended-dictionary+ :extended-dictionary)
(defparameter +origin-corpus+              :corpus)
(defparameter +origin-resource+            :resource)
(defparameter +origin-unknown-word+        :unknown-word)
(defparameter +origin-tankan+              :tankan)

;; word-pron pair n-gram model
(defstruct kkc
  n-gram-model
  vocabulary
  word-dictionary
  char-dictionary
  extended-dictionary)

;;; Convert
(defgeneric get-convert-score-fn (kkc)
  (:documentation "Returns a score function for conversion"))

(defun make-unknown-word-unit (pron)
  (hachee.kkc.dictionary:make-unit
   :pron pron
   :form (hachee.ja:hiragana->katakana pron)))

(defun list-entries (sub-pron &key dictionary vocabulary)
  (let ((entries
         (mapcar
          (lambda (dictionary-entry)
            (make-instance 'hachee.kkc.convert:dictionary-entry
             :entry dictionary-entry
             :token (hachee.language-model.vocabulary:to-int-or-unk
                     vocabulary
                     (hachee.kkc.dictionary:unit->key
                      (hachee.kkc.dictionary:entry-unit dictionary-entry)))))
          (hachee.kkc.dictionary:lookup dictionary sub-pron))))
    ;; Add unknown word node if necessary
    (when (< (length sub-pron) 8) ;; Length up to 8
      (let ((unk-unit (make-unknown-word-unit sub-pron)))
        (when (not (hachee.kkc.dictionary:contains-p dictionary unk-unit))
          (push (make-instance 'hachee.kkc.convert:non-dictionary-entry
                 :unit unk-unit
                 :token (hachee.language-model.vocabulary:to-int
                         vocabulary
                         hachee.language-model.vocabulary:+UNK+)
                 :origin +origin-unknown-word+)
                entries))))
    (nreverse entries)))

(defun convert (kkc pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi:execute pronunciation
   :begin-entry
   (make-instance 'hachee.kkc.convert:non-dictionary-entry
    :unit hachee.language-model.vocabulary:+BOS+
    :token (hachee.language-model.vocabulary:to-int
            (kkc-vocabulary kkc)
            hachee.language-model.vocabulary:+BOS+)
    :origin +origin-vocabulary+)
   :end-entry
   (make-instance 'hachee.kkc.convert:non-dictionary-entry
    :unit hachee.language-model.vocabulary:+EOS+
    :token (hachee.language-model.vocabulary:to-int
            (kkc-vocabulary kkc)
            hachee.language-model.vocabulary:+EOS+)
    :origin +origin-vocabulary+)
   :score-fn
   (get-convert-score-fn kkc)
   :list-entries-fn
   (lambda (sub-pron)
     (list-entries sub-pron
                   :dictionary (kkc-word-dictionary kkc)
                   :vocabulary (kkc-vocabulary kkc)))
   :1st-boundary-index 1st-boundary-index))


;;; Lookup
(defgeneric get-lookup-score-fn (kkc prev-word next-word)
  (:documentation "Returns a score function for lookup"))

(defmethod get-lookup-score-fn ((kkc kkc) prev-word next-word)
  nil)

(defun lookup (kkc pronunciation &key prev next)
  (hachee.kkc.lookup:execute pronunciation
   :score-fn (when (and next prev)
               (get-lookup-score-fn kkc prev next))
   :word-dict (kkc-word-dictionary kkc)
   :char-dict (kkc-char-dictionary kkc)))
