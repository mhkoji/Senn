;;; Under construction
(defpackage :senn.fcitx.im2.process-input
  (:use :cl)
  (:export :execute))
(in-package :senn.fcitx.im2.process-input)

(defstruct state chars)

(defgeneric execute (ime state key))

(defmethod execute ((ime senn.im2:ime) (s state)
                    (key senn.fcitx.keys:key))
  (cond ((senn.fcitx.keys:char-p key)
         (let ((char (code-char
                      (senn.fcitx.keys:key-sym key))))
           (senn.im2:input-char ime s char)))))
