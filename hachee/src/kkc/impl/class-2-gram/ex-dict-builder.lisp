(defpackage :hachee.kkc.impl.class-2-gram.ex-dict-builder
  (:use :cl)
  (:export :item-pron
           :item-form
           :list-items
           :kkc-vocabulary-probability
           :kkc-probability
           :kkc-contains-p
           :build))
(in-package :hachee.kkc.impl.class-2-gram.ex-dict-builder)

(defgeneric kkc-vocabulary-probability (kkc))
(defgeneric kkc-probability (kkc string))
(defgeneric kkc-contains-p (kkc item))

(defgeneric list-items (source))
(defgeneric item-pron (item))
(defgeneric item-form (item))

(defun build (kkc source)
  (let ((hash (make-hash-table :test #'equal)))
    (let* ((items
            (remove-if (lambda (item)
                         (kkc-contains-p kkc item))
                       (list-items source)))
           (each-added-probability
            (/ (kkc-vocabulary-probability kkc) (length items))))
      (loop for item in items
            for form = (item-form item)
            for pron = (item-pron item)
            for prob = (kkc-probability kkc form)
            for new-prob = (+ prob each-added-probability)
            for entry = (hachee.kkc.impl.class-2-gram.ex-dict:make-entry
                         :form form
                         :unk-log-probability (log new-prob))
            do (progn (push entry (gethash pron hash)))))
    (hachee.kkc.impl.class-2-gram.ex-dict:make-ex-dict :hash hash)))
