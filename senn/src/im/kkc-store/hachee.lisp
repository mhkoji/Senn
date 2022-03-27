(defpackage :senn.im.kkc-store.hachee
  (:use :cl)
  (:export :make-store))
(in-package :senn.im.kkc-store.hachee)

(defstruct (store (:constructor %make-store))
  kkc
  hachee-impl-lm-kkc)

(defun make-store (hachee-impl-lm-kkc)
  (let ((extended-dictionary nil)) ;; TODO
    (%make-store
     :kkc (make-instance 'senn.im.kkc.hachee:kkc
           :hachee-impl-lm-kkc hachee-impl-lm-kkc
           :extended-dictionary extended-dictionary)
     :hachee-impl-lm-kkc hachee-impl-lm-kkc)))

(defmethod senn.im.kkc-store:get-kkc ((store store))
  (store-kkc store))

(defmethod senn.im.kkc-store:reload ((store store))
  (let ((hachee-impl-lm-kkc (store-hachee-impl-lm-kkc store))
        (extended-dictionary nil)) ;; TODO
    (setf (store-kkc store)
          (make-instance 'senn.im.kkc.hachee:kkc
           :hachee-impl-lm-kkc hachee-impl-lm-kkc
           :extended-dictionary extended-dictionary)))
  (values))
