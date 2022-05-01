(defpackage :senn.bin.senn-ibus
  (:use :cl)
  (:export :main))
(in-package :senn.bin.senn-ibus)

(defun main ()
  (let ((ime (senn.ibus.stateful-ime:engine-make-ime
              (senn.im.kkc.engine:make-engine-runner
               :program "/usr/lib/senn/kkc-engine"))))
    (unwind-protect
         (senn-ipc.server.stdio:start-server
          (lambda (line)
            (senn.ibus.server:handle-request ime line)))
      (senn.ibus.stateful-ime:engine-close-ime ime))))
