(defpackage :senn.bin.senn-ibus
  (:use :cl)
  (:export :main))
(in-package :senn.bin.senn-ibus)

(defun main ()
  (senn.im.mixin.engine:with-engine
      (engine (senn.im.mixin.engine:make-engine-runner
               :program "/usr/lib/senn/kkc-engine"))
    (let ((ime (senn.ibus.stateful-ime:make-engine-ime engine)))
      (senn.server.stdio:start-server
       (lambda (line)
         (senn.ibus.server:handle-request ime line))))))
