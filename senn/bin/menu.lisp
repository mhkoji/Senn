(defpackage :senn.bin.menu
  (:use :cl)
  (:export :main))
(in-package :senn.bin.menu)
(ql:quickload :senn-gui :silent t)

(defun main ()
  (funcall #+sbcl
           #'sb-thread:join-thread
           #+ccl
           #'ccl:join-process
           (senn.gui.menu.about:show)))
