;; convert/list-candidates depending on hachee kkc impl lm
(defpackage :senn.im.kkc.hachee
  (:use :cl)
  (:export :kkc
           :build-kkc
           :make-state))
(in-package :senn.im.kkc.hachee)

(defun build-kkc ()
  (let ((corpus-pathnames
         (cl-fad:list-directory
          (merge-pathnames
           "src/kkc/data/aozora/word-pron-utf8/"
           (funcall (read-from-string "asdf:system-source-directory")
                    :hachee-kkc)))))
    (log:debug "Loading: ~A" corpus-pathnames)
    (hachee.kkc.impl.lm:build-kkc-simple corpus-pathnames)))

;;;

(defstruct state entries)

(defclass kkc ()
  ((convert
    :initarg :convert
    :reader kkc-convert)
   (state
    :initarg :state
    :reader kkc-state)))

(defmethod senn.im.kkc:convert ((kkc kkc) (pron string)
                                &key 1st-boundary-index)
  (with-accessors ((entries state-entries)) (kkc-state kkc)
    (setf entries (hachee.kkc.convert:execute
                   (kkc-convert kkc) pron
                   :1st-boundary-index 1st-boundary-index))
    (mapcar (lambda (e)
              (senn.im.kkc:make-segment
               :pron (hachee.kkc.convert:entry-pron e)
               :form (hachee.kkc.convert:entry-form e)))
            entries)))

(defmethod senn.im.kkc:list-candidates ((kkc kkc) (index number))
  (with-accessors ((entries state-entries)) (kkc-state kkc)
    (when (and (<= 0 index) (< index (length entries)))
      (let ((pron (hachee.kkc.convert:entry-pron (elt entries index))))
        (mapcar (lambda (item)
                  (senn.im.kkc:make-candidate
                   :form (hachee.kkc.lookup:item-form item)))
                (hachee.kkc.lookup:execute (kkc-lm-impl kkc) pron))))))
