(defpackage :senn.ibus.im
  (:use :cl)
  (:export :ime
           :ime-kkc)
  (:import-from :senn.fcitx.im
                :ime-kkc))
(in-package :senn.ibus.im)

(defclass ime (senn.fcitx.im:ime)
  ())

(defmethod senn.fcitx.im:ime-max-candidate-count ((ime ime))
  ;; An error occurs if the candidate count >= 16.
  15)
