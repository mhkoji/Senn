(defpackage :hachee.kkc.impl.markov.ex-dict-builder
  (:use :cl)
  (:export :item-pron
           :item-form
           :list-items
           :build))
(in-package :hachee.kkc.impl.markov.ex-dict-builder)

(defgeneric list-items (source))
(defgeneric item-pron (item))
(defgeneric item-form (item))

(defun list-items-in-dict-excluded (source in-dict)
  (labels ((in-dict-contains-p (item)
             (let ((form (item-form item))
                   (pron (item-pron item)))
               (find form (hachee.kkc.impl.markov.in-dict:list-entries
                           in-dict pron)
                     :test #'string=
                     :key #'hachee.kkc.impl.markov.in-dict:entry-form))))
    (remove-if #'in-dict-contains-p (list-items source))))

(defun build (source in-dict in-dict-prob char-based-cost-fn)
  (let ((hash (make-hash-table :test #'equal)))
    (let* ((items (list-items-in-dict-excluded source in-dict))
           (each-added-probability (/ in-dict-prob (length items))))
      (loop for item in items
            for form = (item-form item)
            for pron = (item-pron item)
            for cost = (funcall char-based-cost-fn form)
            for prob = (hachee.kkc.impl.markov.cost:->probability cost)
            for new-prob = (+ prob each-added-probability)
            for new-cost = (hachee.kkc.impl.markov.cost:<-probability
                            new-prob)
            for entry = (hachee.kkc.impl.markov.ex-dict:make-entry
                         :form form :cost new-cost)
            do (progn (push entry (gethash pron hash)))))
    (hachee.kkc.impl.markov.ex-dict:make-ex-dict :hash hash)))
