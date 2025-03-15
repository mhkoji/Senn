(defpackage :senn.fcitx.im.immutable.state.selecting-from-predictions
  (:use :cl)
  (:export :state
           :state-predictions
           :state-current-index
           :state-current-input
           :state-move!
           :make-state

           :editing-view))
(in-package :senn.fcitx.im.immutable.state.selecting-from-predictions)

(defstruct state predictions current-index)

(defun state-current-input (s)
  (nth (state-current-index s)
       (state-predictions s)))

(defun state-move! (s diff)
  (let ((new-index (+ (state-current-index s) diff)))
    (when (<= 0 new-index
              (1- (length (state-predictions s))))
      (setf (state-current-index s) new-index)))
  s)

(defun editing-view (s)
  (let ((input (state-current-input s)))
    (senn.fcitx.im.view:editing
     (senn.fcitx.im.view:length-utf8 input)
     input
     (state-predictions s)
     (state-current-index s)
     "")))
