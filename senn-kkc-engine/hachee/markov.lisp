(defpackage :senn-kkc-engine.hachee.markov
  (:use :cl)
  (:export :set-kkc
           :main))
(in-package :senn-kkc-engine.hachee.markov)

;;; user-dict

(defun kkc-apply-user-dict (kkc)
  (let ((dict (senn-user-dict:read-file)))
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
