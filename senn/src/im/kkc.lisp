(defpackage :senn.im.kkc
  (:use :cl)
  (:export :ime
           :load-kkc))
(in-package :senn.im.kkc)

(defclass ime (senn.im:ime)
  ((kkc :initarg :kkc
        :reader ime-kkc)))

(defmethod senn.im:convert ((ime ime) (pron string))
  (let ((words (hachee.kkc:convert-into-words (ime-kkc ime) pron)))
    (mapcar (lambda (w)
              (senn.segment:make-segment
               :pron (hachee.kkc:word-pron w)
               :forms (list (hachee.kkc:word-form w))
               :has-more-forms-p t
               :current-index 0))
            words)))

(defmethod senn.im:lookup-forms ((ime ime) (pron string))
  (hachee.kkc:lookup-forms (ime-kkc ime) pron))


(defun load-user-kkc (user-homedir-pathname)
  (when user-homedir-pathname
    (let ((kkc-path (merge-pathnames ".senn/kkc.zip"
                                     user-homedir-pathname)))
      (when (cl-fad:file-exists-p kkc-path)
        (hachee.kkc:load-kkc kkc-path)))))

(defun create-system-kkc ()
  (let ((corpus-pathnames
         (cl-fad:list-directory
          (merge-pathnames
           "src/kkc/data/aozora/word-pron-utf8/"
           (asdf:system-source-directory :hachee)))))
    (log:debug "Loading: ~A" corpus-pathnames)
    (hachee.kkc:create-simple-kkc corpus-pathnames)))

(defun load-kkc (&optional user-homedir-pathname)
  (or (load-user-kkc user-homedir-pathname)
      (create-system-kkc)))
