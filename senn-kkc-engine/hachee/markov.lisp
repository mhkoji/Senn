(defpackage :senn-kkc-engine.hachee.markov
  (:use :cl)
  (:export :set-kkc
           :main))
(in-package :senn-kkc-engine.hachee.markov)

(defvar *kkc*)

(defun user-dict-pathname ()
  (merge-pathnames ".senn/user-dict.tsv"
                   (user-homedir-pathname)))

(defun set-kkc (&optional (dir "~/senn-data/"))
  (setq *kkc* (hachee.kkc.impl.markov.read:read-kkc dir)))

(defun main ()
  (ignore-errors
   (hachee.kkc.impl.markov:kkc-apply-user-dict
    *kkc* (user-dict-pathname)))
  (senn-kkc-engine.hachee:run *kkc*
                              *standard-input*
                              *standard-output*))
