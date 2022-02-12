(defpackage :senn.ibus.stateful-ime-engine
  (:use :cl)
  (:export :make-ime
           :close-ime))
(in-package :senn.ibus.stateful-ime-engine)

(defun make-ime (engine-runner)
  (senn.ibus.stateful-ime:make-ime
   :kkc (make-instance 'senn.im.kkc.engine:kkc
         :engine-store
         (senn.im.kkc.engine:make-engine-store
          :engine (senn.im.kkc.engine:run-engine engine-runner)
          :engine-runner engine-runner))))

(defun close-ime (ime)
  (senn.im.kkc.engine:close-kkc (senn.fcitx.im:ime-kkc ime)))
