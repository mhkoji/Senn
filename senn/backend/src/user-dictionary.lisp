(defpackage :senn.user-dictionary
  (:use :cl)
  (:export :load-dictionary
           :item-pron
           :item-form
           :add-to-prefix-dictionary))
(in-package :senn.user-dictionary)

(defstruct item
  pron form)

(defstruct dictionary
  items)

(defun load-dictionary (pathname)
  (when (cl-fad:file-exists-p pathname)
    (make-dictionary
     :items (with-open-file (in-stream pathname)
              (loop for row in (cl-csv:read-csv in-stream)
                    while row
                    when (and (first row) (second row))
                    collect (make-item :pron (first row)
                                       :form (second row)))))))

(defun item-unit (item)
  (hachee.kkc.dictionary:make-unit :form (item-form item)
                                   :pron (item-pron item)))

(defun add-to-prefix-dictionary (dictionary prefix-dictionary)
  (dolist (item (dictionary-items dictionary))
    (senn.prefix-dictionary:add-unit prefix-dictionary (item-unit item))))
