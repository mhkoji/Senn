;; convert/lookup depending on hachee kkc
(defpackage :senn.im.mixin.hachee
  (:use :cl)
  (:export :convert
           :lookup
           :build-kkc))
(in-package :senn.im.mixin.hachee)

(defun convert (convert pron &key 1st-boundary-index)
  (mapcar (lambda (e)
            (senn.segment:make-segment
             :pron
             (hachee.kkc.convert:entry-pron e)
             :candidates
             (list (senn.segment:make-candidate
                    :form (hachee.kkc.convert:entry-form e)
                    :origin (hachee.kkc.convert:entry-origin e)))
             :current-index 0
             :has-more-candidates-p t))
          (hachee.kkc.convert:execute
           convert pron
           :1st-boundary-index 1st-boundary-index)))

(defun lookup (lookup pron &key prev next)
  (mapcar (lambda (item)
            (senn.segment:make-candidate
             :form (hachee.kkc.lookup:item-form item)
             :origin (hachee.kkc.lookup:item-origin item)))
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

(defclass convert ()
  ((kkc-impl
    :initarg :convert-kkc-impl
    :reader convert-kkc-impl)))

(defmethod senn.im:convert ((mixin convert) (pron string)
                            &key 1st-boundary-index)
  (convert (convert-kkc-impl mixin) pron
           :1st-boundary-index 1st-boundary-index))

(defclass lookup ()
  ((kkc-impl
    :initarg :lookup-kkc-impl
    :reader lookup-kkc-impl)))

(defmethod senn.im:lookup ((mixin lookup) (pron string)
                           &key prev next)
  (lookup (lookup-kkc-impl mixin) pron :next next :prev prev))
