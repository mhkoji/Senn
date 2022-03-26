;; convert/list-candidates depending on hachee kkc impl lm
(defpackage :senn.im.kkc.hachee
  (:use :cl)
  (:export :kkc
           :build-kkc))
(in-package :senn.im.kkc.hachee)

(defun build-kkc ()
  (let ((corpus-pathnames
         (hachee.data.corpus:word-pron-utf8-pathnames)))
    (log:debug "Loading: ~A" corpus-pathnames)
    (hachee.kkc.impl.lm:build-kkc-simple corpus-pathnames)))

;;;

(defclass kkc ()
  ((kkc-impl
    :initarg :kkc-impl
    :reader kkc-kkc-impl)))

(defmethod senn.im.kkc:convert ((kkc kkc) (pron string)
                                &key 1st-boundary-index)
  (let ((entries (hachee.kkc.convert:execute
                  (kkc-kkc-impl kkc) pron
                  :1st-boundary-index 1st-boundary-index)))
    (mapcar (lambda (e)
              (let ((pron (hachee.kkc.convert:entry-pron e))
                    (form (hachee.kkc.convert:entry-form e)))
                (senn.im.kkc:make-segment
                 :pron pron
                 :candidates (list (senn.im.kkc:make-candidate
                                    :form form)))))
            entries)))

(defmethod senn.im.kkc:list-candidates ((kkc kkc) (pron string))
  (let ((items (hachee.kkc.lookup:execute (kkc-kkc-impl kkc) pron)))
    (mapcar (lambda (item)
              (senn.im.kkc:make-candidate
               :form (hachee.kkc.lookup:item-form item)))
            items)))
