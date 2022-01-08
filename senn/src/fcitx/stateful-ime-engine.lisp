(defpackage :senn.fcitx.stateful-ime-engine
  (:use :cl)
  (:export :make-ime
           :close-ime))
(in-package :senn.fcitx.stateful-ime-engine)

(defclass ime (senn.fcitx.stateful-ime:ime
               senn.im.kkc.engine:convert
               senn.im.kkc.engine:lookup)
  ())

(defun make-ime (engine-runner)
  (make-instance 'ime
   :state (senn.fcitx.stateful-ime:make-initial-state)
   :engine-store (senn.im.kkc.engine:make-engine-store
                  :engine (senn.im.kkc.engine:run-engine
                           engine-runner)
                  :engine-runner engine-runner)))

(defun close-ime (ime)
  (senn.im.kkc.engine:close-mixin ime))
