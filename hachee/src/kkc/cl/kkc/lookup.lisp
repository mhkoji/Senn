(defpackage :hachee.kkc.lookup
  (:use :cl)
  (:export :execute
           :item-form
           :item-origin))
(in-package :hachee.kkc.lookup)

(defstruct item form origin)

(defun lookup-word-forms (dict pron)
  (mapcar #'hachee.kkc.word:word-form
          (hachee.kkc.word.dictionary:lookup dict pron)))

(defun lookup-char-forms (dict pron)
  (mapcar #'hachee.kkc.word:char-form
          (hachee.kkc.word.dictionary:lookup dict pron)))

(defun execute (pronunciation &key word-dicts char-dicts)
  (let ((result-items nil))
    (labels ((push-forms (forms origin)
               (dolist (form forms)
                 (let ((item (make-item :form form :origin origin)))
                   (pushnew item result-items
                            :test #'string=
                            :key #'item-form)))))
      (loop for (origin dict) in word-dicts do
        (push-forms (lookup-word-forms dict pronunciation) origin))
      (loop for (origin dict) in char-dicts do
        (push-forms (lookup-char-forms dict pronunciation) origin)))
    (nreverse result-items)))
