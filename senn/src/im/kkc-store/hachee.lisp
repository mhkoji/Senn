(defpackage :senn.im.kkc-store.hachee
  (:use :cl)
  (:export :make-store))
(in-package :senn.im.kkc-store.hachee)

(defun make-store (kkc-impl)
  (make-instance 'senn.im.kkc.hachee:kkc
                 :kkc-impl kkc-impl))

(defmethod senn.im.kkc-store:get-kkc ((this senn.im.kkc.hachee:kkc))
  this)

(defmethod senn.im.kkc-store:reload ((this senn.im.kkc.hachee:kkc))
  nil)
