(defpackage :hachee.kkc.dictionary
  (:use :cl)
  (:export :make-unit
           :unit-form
           :unit-pron
           :unit=
           :unit->key
           :unit->pron-units

           :dictionary
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
           :contains-p

           :+origin-vocabulary+
           :+origin-corpus+
           :+origin-resource+))
(in-package :hachee.kkc.dictionary)

;; unit = form/pron
(defun make-unit (&key form pron)
  (concatenate 'string form "/" pron))

(defun unit-form (unit)
  (first (cl-ppcre:split "/" unit)))

(defun unit-pron (unit)
  (second (cl-ppcre:split "/" unit)))

(defun unit= (unit1 unit2)
  (string= unit1 unit2))

(defun unit->key (unit)
  unit)

(defun unit->pron-units (unit)
  (loop for ch across (unit-pron unit)
        collect (make-unit :form (string ch)
                           :pron (string ch))))

;;;
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
  (let ((key (unit-pron unit))
        (entry (make-entry :unit unit :origin origin)))
    (pushnew entry (gethash pron (dictionary-entry-hash dict))
             :key #'entry-unit
             :test #'unit=)
    (values)))

(defun lookup (dictionary pron)
  (gethash pron (dictionary-word-hash dictionary)))

(defun contains-p (dictionary unit)
  (let ((entries (lookup dictionary (unit-pron unit))))
    (member unit entries :key #'entry-unit :test #'unit=)))


(defparameter +origin-vocabulary+ :vocabulary)
(defparameter +origin-corpus+     :corpus)
(defparameter +origin-resource+   :resource)
