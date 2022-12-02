(defpackage :senn-kkc-engine.hachee.engine.lm
  (:use :cl)
  (:export :set-kkc
           :main))
(in-package :senn-kkc-engine.hachee.engine.lm)

(defvar *kkc*)

(defun set-kkc (kkc)
  (setq *kkc* kkc))

(defun main ()
  (senn-ipc.server.stdio:start-server
   (lambda (line)
     (senn-kkc-engine.hachee.engine:handle line *kkc*))))
