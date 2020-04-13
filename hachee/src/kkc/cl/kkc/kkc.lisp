(defpackage :hachee.kkc
  (:use :cl)
  (:export :kkc
           :convert
           :get-convert-score-fn
           :lookup
           :get-lookup-score-fn
           :profile))
(in-package :hachee.kkc)

;; word-pron pair n-gram model
(defstruct kkc
  n-gram-model
  vocabulary
  word-dictionary
  char-dictionary)

;;; Convert
(defgeneric get-convert-score-fn (kkc)
  (:documentation "Returns a score function for conversion"))

(defun make-unknown-word-unit (pron)
  (make-word :pron pron :form (hachee.ja:hiragana->katakana pron)))

(defun list-entries (sub-pron &key dictionary vocabulary)
  (let ((entries
         (mapcar
          (lambda (dictionary-entry)
            (make-instance 'hachee.kkc.entry:dictionary-entry
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
          (push (make-instance 'hachee.kkc.entry:unknown-word-entry
                 :unit unk-unit
                 :token (hachee.language-model.vocabulary:to-int
                         vocabulary
                         hachee.language-model.vocabulary:+UNK+))
                entries))))
    (nreverse entries)))

(defun convert (kkc pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert.viterbi:execute pronunciation
   :begin-entry
   (hachee.kkc.entry:make-entry
    :word hachee.language-model.vocabulary:+BOS+
    :token (hachee.language-model.vocabulary:to-int
            (kkc-vocabulary kkc)
            hachee.language-model.vocabulary:+BOS+)
    :origin :vocabulary)
   :end-entry
   (hachee.kkc.entry:make-entry
    :word hachee.language-model.vocabulary:+EOS+
    :token (hachee.language-model.vocabulary:to-int
            (kkc-vocabulary kkc)
            hachee.language-model.vocabulary:+EOS+)
    :origin :vocabulary)
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
