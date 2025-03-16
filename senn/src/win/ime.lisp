(defpackage :senn.win.ime
  (:use :cl)
  (:export :toggle-input-mode
           :can-process
           :process-input))
(in-package :senn.win.ime)

(defgeneric toggle-input-mode (state ime input-mode))

(defgeneric can-process (state ime key input-mode))

(defgeneric process-input (state ime key input-mode))
