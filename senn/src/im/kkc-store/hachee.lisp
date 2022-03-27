(defpackage :senn.im.kkc-store.hachee
  (:use :cl)
  (:export :make-store))
(in-package :senn.im.kkc-store.hachee)

(defstruct (store (:constructor %make-store))
  user-dict ;; TODO
  kkc
  hachee-impl-lm-kkc)

(defun build-ex-dict (user-dict hachee-impl-lm-kkc)
  (when user-dict
    (hachee.kkc.impl.lm.ex-dict-builder:build
     user-dict
     (hachee.kkc.impl.lm:kkc-word-dictionary hachee-impl-lm-kkc))))

(defun make-store (hachee-impl-lm-kkc &optional user-dict)
  (%make-store
   :kkc (make-instance 'senn.im.kkc.hachee:kkc
         :hachee-impl-lm-kkc hachee-impl-lm-kkc
         :extended-dictionary (build-ex-dict user-dict
                                             hachee-impl-lm-kkc))
   :hachee-impl-lm-kkc hachee-impl-lm-kkc))

(defmethod senn.im.kkc-store:get-kkc ((store store))
  (store-kkc store))

(defmethod senn.im.kkc-store:reload ((store store))
  (let ((user-dict  (store-user-dict store)) ;; TODO
        (hachee-impl-lm-kkc (store-hachee-impl-lm-kkc store)))
    (setf (store-kkc store)
          (make-instance 'senn.im.kkc.hachee:kkc
           :hachee-impl-lm-kkc hachee-impl-lm-kkc
           :extended-dictionary (build-ex-dict user-dict
                                               hachee-impl-lm-kkc))))
  (values))
