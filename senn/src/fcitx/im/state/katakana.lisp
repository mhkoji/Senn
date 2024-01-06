(defpackage :senn.fcitx.im.state.katakana
  (:use :cl)
  (:export :state
           :state-input
           :make-state
           :editing-view))
(in-package :senn.fcitx.im.state.katakana)

(defstruct (state (:constructor %make-state))
  (input ""))

(defun make-state (&key input)
  (%make-state :input (senn.ja:hiragana->katakana input)))

(defun editing-view (s)
  (senn.fcitx.im.view:editing-by-string (state-input s) nil nil))
