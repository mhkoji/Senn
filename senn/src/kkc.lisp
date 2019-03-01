;; Provide the interface of kkc
(defpackage :senn.kkc
  (:use :cl :hachee.kkc)
  (:export :convert
           :lookup
           :word-pron
           :word-form
           :load-kkc)
  (:shadow :load-kkc))
(in-package :senn.kkc)

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
    (create-kkc corpus-pathnames)))

(defun load-kkc (&optional user-homedir-pathname)
  (or (load-user-kkc user-homedir-pathname)
      (create-system-kkc)))
