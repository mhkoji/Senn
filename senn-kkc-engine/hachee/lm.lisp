(defpackage :senn-kkc-engine.hachee.lm
  (:use :cl)
  (:export :main))
(in-package :senn-kkc-engine.hachee.lm)

(defun create-system-kkc ()
  (let ((corpus-pathnames
         (cl-fad:list-directory
          (merge-pathnames
           "src/kkc/data/aozora/word-pron-utf8/"
           (funcall (read-from-string "asdf:system-source-directory")
                    :hachee-kkc)))))
    (log:debug "Loading: ~A" corpus-pathnames)
    (hachee.kkc.impl.lm:build-kkc-simple corpus-pathnames)))

(defvar *kkc*
  (create-system-kkc))

(defun main ()
  (senn-kkc-engine.hachee:run *kkc*
                              *standard-input*
                              *standard-output*))
