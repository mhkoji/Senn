(defpackage :senn.fcitx.stateful-ime-hachee
  (:use :cl)
  (:export :make-ime))
(in-package :senn.fcitx.stateful-ime-hachee)

(defun make-ime (kkc-impl)
  (senn.fcitx.stateful-ime:make-ime
   :kkc-store (senn.im.kkc-store.hachee:make-store kkc-impl)
   :predictor (make-instance 'senn.im.predict.katakana:predictor)))
