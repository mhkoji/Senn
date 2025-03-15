(defpackage :senn.fcitx.im.immutable.state.inputting
  (:use :cl :senn.im.inputting)
  (:export :mixin
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
(in-package :senn.fcitx.im.immutable.state.inputting)

(defun buffer-cursor-pos-utf8 (buffer)
  (let ((string (senn.im.buffer:buffer-string buffer))
        (cursor-pos (senn.im.buffer:buffer-cursor-pos buffer)))
    (senn.fcitx.im.view:length-utf8 (subseq string 0 cursor-pos))))

(defun editing-view (s &key committed-string)
  (let ((buffer (state-buffer s)))
    (senn.fcitx.im.view:editing (buffer-cursor-pos-utf8 buffer)
                                (senn.im.buffer:buffer-string buffer)
                                (state-predictions s)
                                nil
                                (or committed-string ""))))
