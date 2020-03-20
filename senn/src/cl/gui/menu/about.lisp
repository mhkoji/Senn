(defpackage :senn.gui.menu.about
  (:use :cl)
  (:export :show))
(in-package :senn.gui.menu.about)
(named-readtables:in-readtable :qt)

(defclass dialog ()
  ()
  (:metaclass qt:qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((dialog dialog) &key)
  (qt:new dialog)
  (#_setFixedSize dialog 640 300)
  (#_setWindowTitle dialog "About Senn")
  (#_move (#_new QLabel "Senn is an input method editor for the Japanese language." dialog) 20 20))

(defun show ()
  (qt:with-main-window (dialog (make-instance 'dialog))))
