(defpackage :senn-kkc-engine.hachee.markov
  (:use :cl)
  (:export :set-kkc
           :main))
(in-package :senn-kkc-engine.hachee.markov)

;;; user-dict

(defun user-dict-pathname ()
  (merge-pathnames ".senn/user-dict.tsv"
                   (user-homedir-pathname)))

(defmethod hachee.kkc.impl.markov.ex-dict-builder:item-pron
    ((item senn.im.user-dict:entry))
  (senn.im.user-dict:entry-pron item))

(defmethod hachee.kkc.impl.markov.ex-dict-builder:item-form
    ((item senn.im.user-dict:entry))
  (senn.im.user-dict:entry-form item))

(defmethod hachee.kkc.impl.markov.ex-dict-builder:list-items
    ((source senn.im.user-dict:dict))
  (senn.im.user-dict:dict-entries source))

(defun kkc-apply-user-dict (kkc)
  (let ((dict (senn.im.user-dict:read-file (user-dict-pathname))))
    (when dict
      (hachee.kkc.impl.markov:kkc-set-ex-dict kkc dict))))

;;;

(defvar *kkc*)

(defun set-kkc (&optional (dir "~/senn-data/"))
  (setq *kkc* (hachee.kkc.impl.markov.read:read-kkc-dir dir)))

(defun main ()
  (ignore-errors
   (kkc-apply-user-dict *kkc*))
  (senn-kkc-engine.hachee:run *kkc*
                              *standard-input*
                              *standard-output*))
