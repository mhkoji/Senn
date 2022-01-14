;; convert/list-candidates depending on hachee kkc
(defpackage :senn.im.kkc.hachee
  (:use :cl)
  (:export :kkc
           :build-kkc
           :make-holder))
(in-package :senn.im.kkc.hachee)

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

(defstruct holder entries)

(defclass kkc ()
  ((impl
    :initarg :impl
    :reader kkc-impl)
   (exteded-dictionary
    :initarg :extended-dictionary
    :initform nil
    :reader kkc-extended-dictionary)
   (holder
    :initarg :holder
    :reader kkc-holder)))

(defun kkc-convert (kkc)
  (let ((ex-dict (kkc-extended-dictionary kkc)))
    (if ex-dict
        (hachee.kkc:make-kkc-convert
         :kkc (kkc-impl kkc)
         :extended-dictionary ex-dict)
        (kkc-impl kkc))))

(defmethod senn.im.kkc:convert ((kkc kkc) (pron string)
                                &key 1st-boundary-index)
  (with-accessors ((entries holder-entries)) (kkc-holder kkc)
    (setf entries (hachee.kkc.convert:execute
                   (kkc-convert kkc) pron
                   :1st-boundary-index 1st-boundary-index))
    (mapcar (lambda (e)
              (senn.im.kkc:make-segment
               :pron (hachee.kkc.convert:entry-pron e)
               :form (hachee.kkc.convert:entry-form e)))
            entries)))

(defmethod senn.im.kkc:list-candidates ((kkc kkc) (index number))
  (with-accessors ((entries holder-entries)) (kkc-holder kkc)
    (when (and (<= 0 index) (< index (length entries)))
      (let ((pron (hachee.kkc.convert:entry-pron (elt entries index))))
        (mapcar (lambda (item)
                  (senn.im.kkc:make-candidate
                   :form (hachee.kkc.lookup:item-form item)))
                (hachee.kkc.lookup:execute (kkc-impl kkc) pron))))))
