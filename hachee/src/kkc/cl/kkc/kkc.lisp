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

(defun get-list-words-fn (kkc)
  (lambda (sub-pron)
    (list
     (cons :vocabulary
           (hachee.kkc.word.dictionary:lookup
            (kkc-vocabulary-dictionary kkc)
            sub-pron))
     (cons :extended-dictionary
           (hachee.kkc.word.dictionary:lookup
            (kkc-extended-dictionary kkc)
            sub-pron))
     (cons :unknown-word
           ;; Add unknown word node if necessary
           (when (< (length sub-pron) 8) ;; Length up to 8
             (let ((unknown-word (make-unknown-word sub-pron)))
               (when (null (hachee.language-model.vocabulary:to-int-or-nil
                            (kkc-vocabulary kkc)
                            unknown-word))
                 (list unknown-word))))))))

(defun convert (kkc pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert:execute pronunciation
   :score-fn (get-convert-score-fn kkc)
   :list-words-fn (get-list-words-fn kkc)
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
