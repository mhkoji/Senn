;; The input-processor module processes user (keyboard) inputs.
;; This process is described by state transition that includes:
;;  - Latin-to-Hiragana conversion
;;  - Kana-Kanji Conversion
(defpackage :senn.fcitx.input-processor
  (:use :cl)
  (:export :process-input
           :make-initial-state
           :+IRV-TO-PROCESS+
           :+IRV-DO-NOTHING+
           :+IRV-FLAG-FORWARD-KEY+))
(in-package :senn.fcitx.input-processor)

(defgeneric process-input (ime state key))

(defgeneric make-initial-state (ime))

(defparameter +IRV-TO-PROCESS+       :IRV_TO_PROCESS)
(defparameter +IRV-DO-NOTHING+       :IRV_DO_NOTHING)
(defparameter +IRV-FLAG-FORWARD-KEY+ :IRV_FLAG_FORWARD_KEY)
