(defpackage :senn.fcitx.stateful-ime-hachee
  (:use :cl)
  (:export :make-ime))
(in-package :senn.fcitx.stateful-ime-hachee)

(defclass ime (senn.fcitx.stateful-ime:ime
               senn.im.kkc.hachee:convert
               senn.im.kkc.hachee:lookup
               senn.im.predict.katakana:predict)
  ())

(defun make-ime (kkc)
  (let ((state (senn.fcitx.stateful-ime:make-initial-state)))
    (make-instance 'ime :state state :kkc kkc)))
