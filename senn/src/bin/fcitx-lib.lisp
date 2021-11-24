(defpackage :senn.bin.fcitx-lib
  (:use :cl)
  (:export :make-ime
           :process-input))
(in-package :senn.bin.fcitx-lib)

(defvar *kkc* (senn.im.kkc:load-kkc))

(defun make-ime ()
  (senn.fcitx.stateful-ime:make-kkc-ime *kkc*))

(defun process-input (stateful-ime sym state)
  (senn.fcitx.stateful-ime:process-input
   stateful-ime
   (senn.fcitx.keys:make-key :sym sym :state state)))
