(defpackage :senn.bin.fcitx-server
  (:use :cl)
  (:export :main
           :run))
(in-package :senn.bin.fcitx-server)

(defun start-server (ime)
  (senn-ipc.server.stdio:start-server
   (lambda (line)
     (senn.fcitx.im.server:handle-request ime line))))

(defun run (engine-path)
  (let ((kkc (senn.im.kkc.engine:start-kkc
              (senn.im.kkc.engine:make-engine-runner
               :program engine-path))))
    (unwind-protect
         (start-server (senn.fcitx.im.mutable:make-ime :kkc kkc))
      (senn.im.kkc.engine:close-kkc kkc))))

(defun main ()
  (run (second sb-ext:*posix-argv*)))
