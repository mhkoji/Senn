(defpackage :hachee.kkc
  (:use :cl)
  (:export :word-form
           :word-pron

           :kkc
           :convert
           :get-convert-score-fn
           :lookup
           :get-lookup-score-fn
           :profile)
  (:import-from :hachee.kkc.word
                :make-word
                :word-form
                :word-pron))
(in-package :hachee.kkc)

;; word-pron pair n-gram model
(defstruct kkc
  n-gram-model
  vocabulary
  vocabulary-dictionary
  extended-dictionary
  word-dictionary
  tankan-dictionary)

;;; Convert
(defgeneric get-convert-score-fn (kkc)
  (:documentation "Returns a score function for conversion"))

(defun make-unknown-word (pron)
  (make-word :pron pron :form (hachee.ja:hiragana->katakana pron)))

(defun list-entries (sub-pron &key vocabulary-dictionary
                                   extended-dictionary
                                   vocabulary)
  (let ((entries nil))
    (dolist (word (hachee.kkc.word.dictionary:lookup vocabulary-dictionary
                                                     sub-pron))
      (push (hachee.kkc.entry:make-entry
             :word word
             :token (hachee.language-model.vocabulary:to-int
                     vocabulary
                     (hachee.kkc.word:word->key word))
             :origin :vocabulary)
            entries))
    (dolist (word (hachee.kkc.word.dictionary:lookup extended-dictionary
                                                     sub-pron))
      (push (hachee.kkc.entry:make-entry
             :word word
             :token (hachee.language-model.vocabulary:to-int
                     vocabulary
                     hachee.language-model.vocabulary:+UNK+)
             :origin :extended-dictionary)
            entries))
    ;; Add unknown word node if necessary
    (when (< (length sub-pron) 8) ;; Length up to 8
      (let ((unknown-word (make-unknown-word sub-pron)))
        (when (null (hachee.language-model.vocabulary:to-int-or-nil
                     vocabulary unknown-word))
          (push (hachee.kkc.entry:make-entry
                 :word unknown-word
                 :token (hachee.language-model.vocabulary:to-int
                         vocabulary
                         hachee.language-model.vocabulary:+UNK+)
                 :origin :unknown-word)
                entries))))
    (nreverse entries)))

(defun convert (kkc pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert:execute pronunciation
   :begin-entry (hachee.kkc.entry:make-entry
                 :word hachee.language-model.vocabulary:+BOS+
                 :token (hachee.language-model.vocabulary:to-int
                         (kkc-vocabulary kkc)
                         hachee.language-model.vocabulary:+BOS+)
                 :origin :vocabulary)
   :end-entry (hachee.kkc.entry:make-entry
               :word hachee.language-model.vocabulary:+EOS+
               :token (hachee.language-model.vocabulary:to-int
                       (kkc-vocabulary kkc)
                       hachee.language-model.vocabulary:+EOS+)
               :origin :vocabulary)
   :score-fn (get-convert-score-fn kkc)
   :list-entries-fn (lambda (sub-pron)
                      (list-entries sub-pron
                       :vocabulary-dictionary (kkc-vocabulary-dictionary kkc)
                       :extended-dictionary (kkc-extended-dictionary kkc)
                       :vocabulary (kkc-vocabulary kkc)))
   :1st-boundary-index 1st-boundary-index))


;;; Lookup
(defgeneric get-lookup-score-fn (kkc prev-word next-word)
  (:documentation "Returns a score function for lookup"))

(defmethod get-lookup-score-fn ((kkc kkc) prev-word next-word)
  nil)

(defun lookup (kkc pronunciation &key prev next)
  (hachee.kkc.lookup:execute pronunciation
   :score-fn
   (when (and next prev)
     (get-lookup-score-fn kkc prev next))
   :word-dicts
   (list (list :vocabulary
               (kkc-vocabulary-dictionary kkc)
               :extended-dictionary
               (kkc-extended-dictionary kkc)
               :word-dictionary
               (kkc-word-dictionary kkc)))
   :char-dicts
   (list (list :tankan-dictionary
               (kkc-tankan-dictionary kkc)))))
