(defpackage :senn.bin.fcitx-lib
  (:use :cl)
  (:import-from :senn.fcitx.server
                :handle-request)
  (:export :make-ime
           :handle-request))
(in-package :senn.bin.fcitx-lib)

;; TODO: Find a way to build kkc at compile time
(defvar *kkc* nil)

(defun make-ime ()
  (senn.fcitx.stateful-ime:make-kkc-ime *kkc*))
