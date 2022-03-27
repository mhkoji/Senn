(defpackage :hachee.kkc.impl.lm.ex-dict-builder
  (:use :cl)
  (:export :item-pron
           :item-form
           :list-items
           :build))
(in-package :hachee.kkc.impl.lm.ex-dict-builder)

(defgeneric list-items (source))
(defgeneric item-pron (item))
(defgeneric item-form (item))

(defun list-items-in-dict-excluded (source in-dict)
  (labels ((in-dict-contains-p (item)
             (let ((form (item-form item))
                   (pron (item-pron item)))
               (find form (hachee.kkc.impl.lm.dictionary:lookup
                           in-dict pron)
                     :test #'string= :key #'item-form))))
    (remove-if #'in-dict-contains-p (list-items source))))

(defun build (source in-dict)
  (let ((items (list-items-in-dict-excluded source in-dict))
        (ex-dict (hachee.kkc.impl.lm.dictionary:make-dictionary)))
    (loop for item in items
          for form = (item-form item)
          for pron = (item-pron item)
          for unit = (hachee.kkc.impl.lm.unit:make-unit
                      :form form
                      :pron pron)
          do (progn
               (hachee.kkc.impl.lm.dictionary:add-entry
                ex-dict unit hachee.kkc.origin:+extended-dictionary+)))
    ex-dict))
