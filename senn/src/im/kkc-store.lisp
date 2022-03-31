(defpackage :senn.im.kkc-store
  (:use :cl)
  (:export :reload
           :get-kkc))
(in-package :senn.im.kkc-store)

(defgeneric reload (store)
  (:method ((kkc t)) nil))

(defgeneric get-kkc (store)
  (:method ((kkc t)) kkc))
