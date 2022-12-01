(defpackage :senn.im.kkc.store.engine
  (:use :cl)
  (:export :make-store-and-run
           :close-store))
(in-package :senn.im.kkc.store.engine)

;;; senn.im.kkc.engine:kkc can behave as a store by itself.
(defun make-store-and-run (engine-runner)
  (senn.im.kkc.engine:make-kkc-and-run engine-runner))

(defmethod senn.im.kkc.store:get-kkc ((this senn.im.kkc.engine:kkc))
  this)

(defmethod senn.im.kkc.store:reload ((this senn.im.kkc.engine:kkc))
  (senn.im.kkc.engine:kkc-rerun-engine this)
  (values))

(defun close-store (this)
  (senn.im.kkc.engine:close-kkc this))
