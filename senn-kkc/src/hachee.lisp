;; convert/list-candidates depending on hachee kkc impl lm
(defpackage :senn-kkc.hachee
  (:use :cl)
  (:export :kkc
           :build-hachee-impl-lm-kkc))
(in-package :senn-kkc.hachee)

(defun build-hachee-impl-lm-kkc ()
  (let ((corpus-pathnames
         (hachee.data.corpus:word-pron-utf8-pathnames)))
    (log:debug "Loading: ~A" corpus-pathnames)
    (hachee.kkc.impl.lm:build-kkc-simple corpus-pathnames)))

;;;

(defclass kkc ()
  ((hachee-impl-lm-kkc
    :initarg :hachee-impl-lm-kkc
    :reader kkc-hachee-impl-lm-kkc)
   (extended-dictionary
    :initarg :extended-dictionary
    :initform nil
    :reader kkc-extended-dictionary)))

(defun hachee-kkc-convert (kkc)
  (if (kkc-extended-dictionary kkc)
      (hachee.kkc.impl.lm:make-kkc-convert
       :kkc (kkc-hachee-impl-lm-kkc kkc)
       :extended-dictionary (kkc-extended-dictionary kkc))
      (kkc-hachee-impl-lm-kkc kkc)))

(defmethod senn-kkc:convert ((kkc kkc) (pron string)
                                &key 1st-boundary-index)
  (let ((entries (hachee.kkc.convert:execute
                  (hachee-kkc-convert kkc) pron
                  :1st-boundary-index 1st-boundary-index)))
    (mapcar (lambda (e)
              (let ((pron (hachee.kkc.convert:entry-pron e))
                    (form (hachee.kkc.convert:entry-form e)))
                (senn-kkc:make-segment
                 :pron pron
                 :candidates (list (senn-kkc:make-candidate
                                    :form form)))))
            entries)))

(defmethod senn-kkc:list-candidates ((kkc kkc) (pron string))
  (let ((items (hachee.kkc.lookup:execute
                (kkc-hachee-impl-lm-kkc kkc) pron)))
    (mapcar (lambda (item)
              (senn-kkc:make-candidate
               :form (hachee.kkc.lookup:item-form item)))
            items)))
