(defpackage :senn.fcitx.im.immutable.state.katakana
  (:use :cl)
  (:export :state
           :state-input
           :make-state
           :editing-view))
(in-package :senn.fcitx.im.immutable.state.katakana)

(defstruct (state (:constructor %make-state))
  (input ""))

(defun make-state (&key input)
  (%make-state :input (senn.ja:hiragana->katakana input)))

(defun editing-view (s)
  (let ((input (state-input s)))
    (senn.fcitx.im.view:editing
     (senn.fcitx.im.view:length-utf8 input)
     input nil nil "")))
