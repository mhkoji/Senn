(defpackage :senn.bin.fcitx-lib
  (:use :cl)
  (:import-from :senn.fcitx.server
                :handle-request)
  (:export :make-ime
           :handle-request))
(in-package :senn.bin.fcitx-lib)

(defvar *kkc*
  (senn.im.mixin.kkc:load-kkc))

(defun make-ime ()
  (senn.fcitx.stateful-ime:make-kkc-ime *kkc*))
