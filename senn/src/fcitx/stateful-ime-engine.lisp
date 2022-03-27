(defpackage :senn.fcitx.stateful-ime-engine
  (:use :cl)
  (:export :make-ime
           :close-ime))
(in-package :senn.fcitx.stateful-ime-engine)

(defun make-ime (engine-runner)
  (senn.fcitx.stateful-ime:make-ime
   :kkc-store (senn.im.kkc-store.engine:make-store-and-run engine-runner)))

(defun close-ime (ime)
  (senn.im.kkc-store.engine:close-store
   (senn.fcitx.stateful-ime:ime-kkc-store ime)))
