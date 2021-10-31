(defpackage :senn.win.im
  (:use :cl)
  (:export :make-editing
           :editing
           :editing-buffer
           :make-converting
           :converting
           :converting-segments
           :converting-pronunciation
           :converting-current-segment-index))
(in-package :senn.win.im)

;; input state

(defstruct editing
  (buffer (senn.buffer:make-buffer)))

(defstruct converting
  segments
  pronunciation
  (current-segment-index 0))
