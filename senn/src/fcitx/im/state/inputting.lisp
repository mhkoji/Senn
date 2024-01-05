(defpackage :senn.fcitx.im.state.inputting
  (:use :cl :senn.im.inputting)
  (:export :ime
           :ime-predictor
           :ime-max-candidate-count

           :state
           :state-buffer-empty-p
           :state-buffer-string
           :state-buffer-get-pron
           :state-buffer-cursor-pos-move!
           :state-predictions
           :make-state

           :insert-char!
           :insert-char-direct!
           :delete-char!

           :editing-view))
(in-package :senn.fcitx.im.state.inputting)

(defun editing-view (s &key committed-string)
  (senn.fcitx.im.view:editing-by-buffer (state-buffer s)
                                        (state-predictions s)
                                        nil
                                        (or committed-string "")))
