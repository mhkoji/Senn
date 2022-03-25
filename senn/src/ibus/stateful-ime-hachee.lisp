(defpackage :senn.ibus.stateful-ime-hachee
  (:use :cl)
  (:export :make-ime))
(in-package :senn.fcitx.stateful-ime-hachee)

(defun make-ime (kkc)
  (senn.ibus.stateful-ime:make-ime
   :kkc (make-instance 'senn.im.kkc.hachee:kkc
         :lm-impl kkc)))
