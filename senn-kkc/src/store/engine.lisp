(defpackage :senn-kkc.store.engine
  (:use :cl)
  (:export :make-store-and-run
           :close-store))
(in-package :senn-kkc.store.engine)

;;; senn-kkc.engine:kkc can behave as a store by itself.
(defun make-store-and-run (engine-runner)
  (senn-kkc.engine:make-kkc-and-run engine-runner))

(defmethod senn-kkc.store:get-kkc ((this senn-kkc.engine:kkc))
  this)

(defmethod senn-kkc.store:reload ((this senn-kkc.engine:kkc))
  (senn-kkc.engine:kkc-rerun-engine this)
  (values))

(defun close-store (this)
  (senn-kkc.engine:close-kkc this))
