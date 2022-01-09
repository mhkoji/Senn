;; convert/lookup depending on hachee kkc
(defpackage :senn.im.kkc.hachee
  (:use :cl)
  (:export :mixin
           :convert
           :lookup
           :kkc
           :mixin-extended-dictionary
           :build-kkc))
(in-package :senn.im.kkc.hachee)

(defun convert (convert pron &key 1st-boundary-index)
  (mapcar (lambda (e)
            (senn.im.segment:make-segment
             :pron
             (hachee.kkc.convert:entry-pron e)
             :candidates
             (list (senn.im.segment:make-candidate
                    :form (hachee.kkc.convert:entry-form e)))
             :current-index 0
             :has-more-candidates-p t))
          (hachee.kkc.convert:execute
           convert pron
           :1st-boundary-index 1st-boundary-index)))

(defun lookup (lookup pron &key prev next)
  (mapcar (lambda (item)
            (senn.im.segment:make-candidate
             :form (hachee.kkc.lookup:item-form item)))
          (hachee.kkc.lookup:execute lookup pron :prev prev :next next)))

(defun build-kkc ()
  (let ((corpus-pathnames
         (cl-fad:list-directory
          (merge-pathnames
           "src/kkc/data/aozora/word-pron-utf8/"
           (funcall (read-from-string "asdf:system-source-directory")
                    :hachee-kkc)))))
    (log:debug "Loading: ~A" corpus-pathnames)
    (hachee.kkc:build-kkc-simple corpus-pathnames)))

;;;

(defclass mixin ()
  ((kkc
    :initarg :kkc
    :reader mixin-kkc)
   (exteded-dictionary
    :initarg :extended-dictionary
    :initform nil
    :reader mixin-extended-dictionary)))

(defclass convert (mixin) ())

(defmethod senn.im.kkc:convert ((mixin convert) (pron string)
                                &key 1st-boundary-index)
  (let ((ex-dict (mixin-extended-dictionary mixin)))
    (convert (if ex-dict
                 (hachee.kkc:make-kkc-convert
                  :kkc (mixin-kkc mixin)
                  :extended-dictionary ex-dict)
                 (mixin-kkc mixin))
             pron
             :1st-boundary-index 1st-boundary-index)))

(defclass lookup (mixin) ())

(defmethod senn.im.kkc:lookup ((mixin lookup) (pron string)
                               &key prev next)
  (lookup (mixin-kkc mixin) pron
          :next next :prev prev))


(defclass kkc (convert lookup)
  ())
