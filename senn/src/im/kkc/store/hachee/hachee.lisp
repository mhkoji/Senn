(defpackage :senn.im.kkc.store.hachee
  (:use :cl)
  (:export :make-store)
  (:local-nicknames (:user-dict :senn.im.kkc.store.hachee.user-dict)))
(in-package :senn.im.kkc.store.hachee)

(defstruct (store (:constructor %make-store))
  kkc
  hachee-impl-lm-kkc)

(defun build-ex-dict (user-dict hachee-impl-lm-kkc)
  (when user-dict
    (hachee.kkc.impl.lm.ex-dict-builder:build
     user-dict
     (hachee.kkc.impl.lm:kkc-word-dictionary hachee-impl-lm-kkc))))

(defun make-kkc (hachee-impl-lm-kkc)
  (let ((user-dict (user-dict:read-file)))
    (let ((ex-dict (build-ex-dict user-dict hachee-impl-lm-kkc)))
      (make-instance 'senn.im.kkc.hachee:kkc
       :hachee-impl-lm-kkc hachee-impl-lm-kkc
       :extended-dictionary ex-dict))))

(defun make-store (hachee-impl-lm-kkc)
  (%make-store :kkc (make-kkc hachee-impl-lm-kkc)
               :hachee-impl-lm-kkc hachee-impl-lm-kkc))

(defmethod senn.im.kkc.store:get-kkc ((store store))
  (store-kkc store))

(defmethod senn.im.kkc.store:reload ((store store))
  (setf (store-kkc store) (make-kkc (store-hachee-impl-lm-kkc store)))
  (values))

;;; user-dict

(defmethod hachee.kkc.impl.lm.ex-dict-builder:item-pron
    ((item user-dict:entry))
  (user-dict:entry-pron item))

(defmethod hachee.kkc.impl.lm.ex-dict-builder:item-form
    ((item user-dict:entry))
  (user-dict:entry-form item))

(defmethod hachee.kkc.impl.lm.ex-dict-builder:list-items
    ((source user-dict:dict))
  (user-dict:dict-entries source))
