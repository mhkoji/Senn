(defpackage :senn.win.states
  (:use :cl)
  (:export :to-view

           :editing
           :make-editing
           :editing-buffer

           :converting
           :make-converting
           :converting-pronunciation
           :converting-segments
           :converting-current-input

           :committed
           :make-committed
           :committed-input))
(in-package :senn.win.states)

(defgeneric to-view (s))

(defstruct editing
  (buffer (senn.buffer:make-buffer)))


(defstruct converting
  segments
  pronunciation)

(defun converting-current-input (c)
  (format nil "~{~A~}"
          (mapcar #'senn.segment:segment-current-form
                  (converting-segments c))))

(defstruct committed input)
