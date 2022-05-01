(defpackage :senn-kkc-engine.hachee.engine.lm
  (:use :cl)
  (:export :main))
(in-package :senn-kkc-engine.hachee.engine.lm)

(defun create-system-kkc ()
  (let ((corpus-pathnames
         (hachee.data.corpus:word-pron-utf8-pathnames)))
    (log:debug "Loading: ~A" corpus-pathnames)
    (hachee.kkc.impl.lm:build-kkc-simple corpus-pathnames)))

(defvar *kkc*
  (create-system-kkc))

(defun main ()
  (senn-ipc.server.stdio:start-server
   (lambda (line)
     (senn-kkc-engine.hachee.engine:handle line *kkc*))))
