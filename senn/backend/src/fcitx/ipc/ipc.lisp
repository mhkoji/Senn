(defpackage :senn.fcitx.ipc
  (:use :cl)
  (:export :read-request
           :send-response))
(in-package :senn.fcitx.ipc)

(defgeneric read-request (c))
(defgeneric send-response (c resp))
