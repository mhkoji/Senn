(defpackage :senn.im.kkc
  (:use :cl)
  (:export :ime
           :load-kkc))
(in-package :senn.im.kkc)

(defclass ime (senn.im:ime)
  ((kkc :initarg :kkc
        :reader ime-kkc)))

(defmethod senn.im:convert ((ime ime) (pron string) &key 1st-boundary-index)
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
          (hachee.kkc:convert (ime-kkc ime) pron
                              :1st-boundary-index 1st-boundary-index)))

(defmethod senn.im:lookup ((ime ime) (pron string)
                           &key prev next)
  (mapcar (lambda (item)
            (senn.segment:make-candidate
             :form (hachee.kkc.lookup:item-form item)
             :origin (hachee.kkc.lookup:item-origin item)))
          (hachee.kkc:lookup (ime-kkc ime) pron :prev prev :next next)))

(defun load-user-kkc (user-homedir-pathname)
  (when user-homedir-pathname
    (let ((kkc-path (merge-pathnames ".senn/kkc.zip"
                                     user-homedir-pathname)))
      (when (cl-fad:file-exists-p kkc-path)
        (hachee.kkc.full:load-kkc kkc-path)))))

(defun create-system-kkc ()
  (let ((corpus-pathnames
         (cl-fad:list-directory
          (merge-pathnames
           "src/kkc/data/aozora/word-pron-utf8/"
           (asdf:system-source-directory :hachee)))))
    (log:debug "Loading: ~A" corpus-pathnames)
    (hachee.kkc.simple::create-kkc corpus-pathnames)))

(defun load-kkc (&optional user-homedir-pathname)
  (or (load-user-kkc user-homedir-pathname)
      (create-system-kkc)))
