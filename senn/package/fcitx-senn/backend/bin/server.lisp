(defpackage :senn.fcitx-senn.backend.bin
  (:use :cl)
  (:export :main))
(in-package :senn.fcitx-senn.backend.bin)
(ql:quickload :senn-fcitx :silent t)

(defvar *kkc*
  ;; There is no user home directory
  ;; because this script is supposed to be run by Docker.
  (senn.im.kkc:load-kkc))

(defun main (&rest argv)
  (declare (ignorable argv))
  (senn.fcitx.server.unix:start-server *kkc*))
