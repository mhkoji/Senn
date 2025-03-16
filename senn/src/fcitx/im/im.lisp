(defpackage :senn.fcitx.im
  (:use :cl)
  (:export :ime
           :ime-kkc
           :ime-max-candidate-count)
  (:import-from :senn.fcitx.ime
                :make-output)
  (:local-nicknames (:inputting
                     :senn.fcitx.im.state.inputting)
                    (:converting
                     :senn.fcitx.im.state.converting)
                    (:selecting-from-predictions
                     :senn.fcitx.im.state.selecting-from-predictions)
                    (:katakana
                     :senn.fcitx.im.state.katakana)))
(in-package :senn.fcitx.im)

(defclass ime (inputting:mixin
               converting:mixin)
  ((kkc
    :initarg :kkc
    :reader ime-kkc)
   (predictor
    :initarg :predictor
    :initform nil
    :reader ime-predictor)))

(defgeneric ime-max-candidate-count (ime)
  (:method ((ime ime))
    nil))


(defmethod senn.fcitx.ime:make-initial-state ((ime ime))
  (senn.fcitx.im.state.inputting:make-state))

(defmethod senn.fcitx.im.state.inputting:ime-max-candidate-count ((ime ime))
  (ime-max-candidate-count ime))

(defmethod senn.fcitx.im.state.inputting:ime-predictor ((ime ime))
  (ime-predictor ime))

(defmethod senn.fcitx.im.state.converting:ime-max-candidate-count ((ime ime))
  (ime-max-candidate-count ime))

(defmethod senn.fcitx.im.state.converting:ime-kkc ((ime ime))
  (ime-kkc ime))


(defun resp (view &key state)
  (list (make-output view) state))
