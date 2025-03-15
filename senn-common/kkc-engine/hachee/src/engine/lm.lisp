(defpackage :senn-kkc-engine.hachee.engine.lm
  (:use :cl)
  (:export :set-kkc
           :build-kkc-using-hachee-corpus
           :main))
(in-package :senn-kkc-engine.hachee.engine.lm)

(defvar *kkc*)

(defun set-kkc (kkc)
  (setq *kkc* kkc))

(defun build-kkc-using-hachee-corpus ()
  (let ((corpus-pathnames
         (hachee.corpus.data:word-pron-utf8-pathnames)))
    (format *error-output* "Loading: ~A" corpus-pathnames)
    (hachee.kkc.impl.lm.build:build-kkc-simple corpus-pathnames)))

(defun main ()
  (senn-ipc.server.stdio:start-server
   (lambda (line)
     (senn-kkc-engine.hachee.engine:handle line *kkc*))))
