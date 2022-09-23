(defpackage :senn.fcitx.im
  (:use :cl)
  (:export :ime
           :ime-max-candidate-count
           :ime-kkc
           :ime-predictor
           :process-input
           :select-candidate
           :make-initial-state)
  (:import-from :senn.fcitx.im.ime
                :ime
                :ime-max-candidate-count
                :ime-kkc
                :ime-predictor))
(in-package :senn.fcitx.im)

(defun process-input (ime state key)
  (senn.fcitx.im.process-input:execute state key ime))

(defun select-candidate (state index)
  (senn.fcitx.im.select-candidate:execute state index))

(defun make-initial-state ()
  (senn.im.inputting:make-state))
