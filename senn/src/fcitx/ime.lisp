(defpackage :senn.fcitx.ime
  (:use :cl)
  (:export :process-input
           :select-candidate
           :make-initial-state
           :make-output))
(in-package :senn.fcitx.ime)

(defgeneric process-input (state ime key))

(defgeneric select-candidate (state ime key))

(defgeneric make-initial-state (ime))

(defun make-output (view)
  (let ((consumed-p (and view t)))
    (list consumed-p view)))
