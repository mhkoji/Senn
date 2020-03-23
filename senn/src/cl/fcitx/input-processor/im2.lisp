;;; Under construction
(defpackage :senn.fcitx.input-processor.im2
  (:use :cl
        :senn.fcitx.input-processor)
  (:import-from :senn.im2
                :ime))
(in-package :senn.fcitx.input-processor.im2)

(defmethod make-initial-state ((ime ime))
  (senn.im2:make-state))

(defmethod process-input ((ime ime) (s senn.im2:state)
                          (key senn.fcitx.input-processor.keys:key))
  (cond ((senn.fcitx.input-processor.keys:char-p key)
         (let ((char (code-char
                      (senn.fcitx.input-processor.keys:key-sym key))))
           (senn.im2:input-char ime s char)))))
