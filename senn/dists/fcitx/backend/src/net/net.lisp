(defpackage :senn.fcitx.net
  (:use :cl)
  (:export :read-request
           :send-response))
(in-package :senn.fcitx.net)

(defgeneric read-request (c))
(defgeneric send-response (c resp))
