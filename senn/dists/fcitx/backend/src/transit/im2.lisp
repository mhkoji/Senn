;;; Under construction
(defpackage :senn.fcitx.transit.im2
  (:use :cl
        :senn.fcitx.transit)
  (:import-from :senn.im2
                :ime))
(in-package :senn.fcitx.transit.im2)

(defmethod make-initial-state ((ime ime))
  (senn.im2:make-state))

(defmethod transit ((ime ime) (s senn.im2:state)
                    (key senn.fcitx.transit.keys:key))
  (cond ((senn.fcitx.transit.keys:char-p key)
         (let ((char (code-char (senn.fcitx.transit.keys:key-sym key))))
           (senn.im2:input-char ime s char)))))
