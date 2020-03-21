(defpackage :senn.bin.menu
  (:use :cl)
  (:export :main))
(in-package :senn.bin.menu)
(ql:quickload :senn-gui :silent t)

(defun main ()
  (senn.gui.menu.about:show))
