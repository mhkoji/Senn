(defpackage :senn.bin.gui-menu-about
  (:use :cl)
  (:export :main))
(in-package :senn.bin.gui-menu-about)

(defun main ()
  (funcall #+sbcl
           #'sb-thread:join-thread
           #+ccl
           #'ccl:join-process
           (senn.gui.menu.about:show)))
