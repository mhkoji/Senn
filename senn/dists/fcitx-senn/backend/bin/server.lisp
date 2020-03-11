(defpackage :senn.fcitx-senn.backend.bin
  (:use :cl)
  (:export :main))
(in-package :senn.fcitx-senn.backend.bin)
(ql:quickload :senn-fcitx :silent t)

(defun main (&rest argv)
  (declare (ignorable argv))
  (let ((kkc (senn.im.kkc:load-kkc (user-homedir-pathname))))
    (senn.fcitx.ipc.unix:start-server
     (make-instance 'senn.im.kkc:ime :kkc kkc)
     #'senn.fcitx.stateful-im:loop-handling-request)))
