(defpackage :senn.fcitx.stateful-ime-hachee
  (:use :cl)
  (:export :make-ime))
(in-package :senn.fcitx.stateful-ime-hachee)

(defun make-ime (kkc)
  (senn.fcitx.stateful-ime:make-ime
   :kkc (make-instance 'senn.im.kkc.hachee:kkc
         :impl kkc
         :holder (senn.im.kkc.hachee:make-holder))
   :predictor (make-instance 'senn.im.predict.katakana:predictor)))
