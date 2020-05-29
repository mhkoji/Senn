(defpackage :senn.im.kkc
  (:use :cl)
  (:export :kkc-mixin
           :load-kkc))
(in-package :senn.im.kkc)

(defclass kkc-mixin ()
  ((kkc :initarg :kkc
        :reader kkc-mixin-kkc)))

(defmethod senn.im:convert ((ime kkc-mixin) (pron string)
                            &key 1st-boundary-index)
  (mapcar (lambda (e)
            (senn.segment:make-segment
             :pron
             (hachee.kkc.convert:entry-pron e)
             :candidates
             (list (senn.segment:make-candidate
                    :form (hachee.kkc.convert:entry-form e)
                    :origin (hachee.kkc.convert:entry-origin e)))
             :has-more-candidates-p t
             :current-index 0))
          (hachee.kkc:convert (kkc-mixin-kkc ime) pron
                              :1st-boundary-index 1st-boundary-index)))

(defmethod senn.im:lookup ((ime kkc-mixin) (pron string)
                           &key prev next)
  (mapcar (lambda (item)
            (senn.segment:make-candidate
             :form (hachee.kkc.lookup:item-form item)
             :origin (hachee.kkc.lookup:item-origin item)))
          (hachee.kkc:lookup (kkc-mixin-kkc ime) pron
                             :prev prev
                             :next next)))

(defun load-user-kkc (senn-homedir-pathname)
  (when senn-homedir-pathname
    (let ((kkc-path (merge-pathnames "kkc.zip" senn-homedir-pathname)))
      (when (cl-fad:file-exists-p kkc-path)
        (hachee.kkc:load-kkc kkc-path)))))

(defun create-system-kkc ()
  (let ((corpus-pathnames
         (cl-fad:list-directory
          (merge-pathnames
           "src/kkc/data/aozora/word-pron-utf8/"
           (asdf:system-source-directory :hachee)))))
    (log:debug "Loading: ~A" corpus-pathnames)
    (hachee.kkc.simple::create-kkc corpus-pathnames)))

(defun load-kkc (&optional senn-homedir-pathname)
  (or (load-user-kkc senn-homedir-pathname)
      (create-system-kkc)))
