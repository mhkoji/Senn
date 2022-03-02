(defpackage :hachee.kkc.impl.lm.dictionary
  (:use :cl)
  (:export :dictionary
           :list-all
           :size
           :make-dictionary
           :save-dictionary
           :load-dictionary
           :entry-unit
           :entry-origin
           :entry
           :add-entry
           :lookup
           :contains-p))
(in-package :hachee.kkc.impl.lm.dictionary)

(defstruct dictionary
  (entry-hash (make-hash-table :test #'equal)))

(defun list-all (dictionary)
  (loop for items in (alexandria:hash-table-values
                      (dictionary-entry-hash dictionary))
        nconc (copy-list items)))

(defun size (dictionary)
  (hash-table-size (dictionary-entry-hash dictionary)))

(defun save-dictionary (dict stream)
  (print (list :entry-hash
               (alexandria:hash-table-alist (dictionary-entry-hash dict)))
         stream)
  (values))

(defun load-dictionary (stream)
  (let ((list (read stream)))
    (make-dictionary :entry-hash
                     (alexandria:alist-hash-table
                      (getf list :entry-hash) :test #'equal))))

;;;
(defstruct entry
  unit
  origin)

(defun add-entry (dict unit origin)
  (let ((key (hachee.kkc.impl.lm.unit:unit-pron unit))
        (entry (make-entry :unit unit :origin origin)))
    (pushnew entry (gethash key (dictionary-entry-hash dict))
             :key #'entry-unit
             :test #'hachee.kkc.impl.lm.unit:unit=)
    (values)))

(defun lookup (dictionary pron)
  (gethash pron (dictionary-entry-hash dictionary)))

(defun contains-p (dictionary unit)
  (let ((entries (lookup dictionary
                         (hachee.kkc.impl.lm.unit:unit-pron unit))))
    (member unit entries :key #'entry-unit
                         :test #'hachee.kkc.impl.lm.unit:unit=)))
