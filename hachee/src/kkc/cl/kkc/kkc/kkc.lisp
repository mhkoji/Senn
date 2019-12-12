(defpackage :hachee.kkc
  (:use :cl)
  (:export :word-form
           :word-pron

           :kkc
           :convert
           :convert-into-words
           :get-score-fn
           :lookup
           :lookup-forms
           :profile)
  (:import-from :hachee.kkc.word
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

(defgeneric get-score-fn (kkc)
  (:documentation "Returns a score function for conversion"))

;;; Convert
(defun convert (kkc pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert:execute pronunciation
   :score-fn              (get-score-fn kkc)
   :vocabulary            (kkc-vocabulary kkc)
   :vocabulary-dictionary (kkc-vocabulary-dictionary kkc)
   :extended-dictionary   (kkc-extended-dictionary kkc)
   :1st-boundary-index    1st-boundary-index))

(defun convert-into-words (kkc pronunciation &key 1st-boundary-index)
  (mapcar #'hachee.kkc.convert:node-word
          (convert kkc pronunciation
                   :1st-boundary-index 1st-boundary-index)))


;;; Lookup
(defun lookup (kkc pronunciation)
  (hachee.kkc.lookup:execute pronunciation
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

(defun lookup-forms (kkc pronunciation)
  (mapcar #'hachee.kkc.lookup:item-form
          (lookup kkc pronunciation)))
