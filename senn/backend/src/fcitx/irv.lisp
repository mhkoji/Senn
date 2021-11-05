(defpackage :senn.fcitx.irv
  (:use :cl)
  (:export :+TO-PROCESS+
           :+DO-NOTHING+
           :+FLAG-FORWARD-KEY+))
(in-package :senn.fcitx.irv)

(defparameter +TO-PROCESS+       :IRV_TO_PROCESS)
(defparameter +DO-NOTHING+       :IRV_DO_NOTHING)
(defparameter +FLAG-FORWARD-KEY+ :IRV_FLAG_FORWARD_KEY)
