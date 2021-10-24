(defpackage :senn.win.im
  (:use :cl)
  (:export :make-initial-state
           :process-input
           :can-process

           :make-editing
           :editing-buffer
           :make-converting
           :converting-current-input
           :converting-current-segment
           :converting-move-curret-segment
           :editing-view
           :converting-view
           :committed-view))
(in-package :senn.win.im)

(defgeneric make-initial-state (ime))

(defgeneric process-input (ime state key))

(defgeneric can-process (ime state key))
