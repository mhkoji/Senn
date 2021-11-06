(defpackage :senn.bin.menu-about
  (:use :cl)
  (:export :main))
(in-package :senn.bin.menu-about)
(ql:quickload :senn-about :silent t)

(defun main ()
  (funcall #+sbcl
           #'sb-thread:join-thread
           #+ccl
           #'ccl:join-process
           (senn.menu.about:show)))
