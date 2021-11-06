(defpackage :senn.fcitx-senn.backend.bin
  (:use :cl)
  (:export :main))
(in-package :senn.fcitx-senn.backend.bin)
(ql:quickload (list :senn-fcitx
                    :senn-server-unix)
              :silent t)

(defvar *kkc*
  ;; There is no user home directory
  ;; because this script is supposed to be run by Docker.
  (senn.im.kkc:load-kkc))

(defun main (&rest argv)
  (declare (ignorable argv))
  (senn.server.unix:start-server
   (lambda (client)
     (senn.fcitx.server:loop-handling-request kkc
      :read-fn (lambda ()
                 (senn.server.unix:read-request client))
      :send-fn (lambda (resp)
                 (senn.server.unix:send-response client resp))))))
