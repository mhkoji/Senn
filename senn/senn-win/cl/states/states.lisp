(defpackage :senn.win.states
  (:use :cl)
  (:export :to-view

           :editing
           :make-editing
           :editing-buffer

           :committed
           :make-committed
           :committed-input))
(in-package :senn.win.states)

(defgeneric to-view (s))

(defstruct editing
  (buffer (senn.buffer:make-buffer)))

(defstruct committed input)
