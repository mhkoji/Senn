(defpackage :senn.lib.fcitx
  (:use :cl)
  (:import-from :senn.fcitx.server
                :handle-request)
  (:export :make-ime
           :handle-request))
(in-package :senn.lib.fcitx)

(defvar *kkc*
  (senn.im.mixin.kkc:load-kkc))

(defun make-ime ()
  (senn.fcitx.stateful-ime:make-kkc-ime *kkc*))
