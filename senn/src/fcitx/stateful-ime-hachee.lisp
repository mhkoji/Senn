(defpackage :senn.fcitx.stateful-ime-hachee
  (:use :cl)
  (:export :make-ime))
(in-package :senn.fcitx.stateful-ime-hachee)

(defclass ime (senn.fcitx.stateful-ime:ime)
  ((hachee-kkc
   :initarg :hachee-kkc)
   (predictor
    :initform
    (make-instance 'senn.im.predict.katakana:predictor))))

(defmethod senn.fcitx.im:ime-kkc ((ime ime))
  (slot-value ime 'hachee-kkc))

(defmethod senn.fcitx.im:ime-predictor ((ime ime))
  (slot-value ime 'predictor))

(defun make-ime (kkc)
  (make-instance 'ime
   :state (senn.fcitx.stateful-ime:make-initial-state)
   :hachee-kkc (make-instance 'senn.im.kkc.hachee:kkc :impl kkc)))
