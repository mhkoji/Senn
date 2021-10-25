(defpackage :senn.win.ime
  (:use :cl)
  (:export :make-editing
           :editing
           :editing-buffer

           :make-converting
           :converting
           :converting-segments
           :converting-pronunciation
           :converting-current-segment-index

           :char-p

           :process-input
           :can-process))
(in-package :senn.win.ime)

;;; state

(defstruct editing
  (buffer (senn.buffer:make-buffer)))

(defstruct converting
  segments
  pronunciation
  (current-segment-index 0))

(defun char-p (k)
  (<= (char-code #\A) (senn.win.keys:key-code k) (char-code #\Z)))
