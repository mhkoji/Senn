;; Provide the interface of kkc
(defpackage :senn.kkc
  (:use :cl :hachee.kkc)
  (:shadow :create-kkc)
  (:export :convert
           :lookup
           :word-pron
           :word-form
           :create-kkc))
(in-package :senn.kkc)

(defun create-kkc ()
  (let ((corpus-pathnames
          (cl-fad:list-directory
           (merge-pathnames
            "src/kkc/data/aozora/word-pron-utf8/"
            (asdf:system-source-directory :hachee)))))
    (log:debug "Loading: ~A" corpus-pathnames)
    (hachee.kkc:create-kkc corpus-pathnames)))
