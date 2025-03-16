(defpackage :senn.t.fcitx.hachee
  (:use :cl))
(in-package :senn.t.fcitx.hachee)

(defun converting-state-segment-strings (state)
  (mapcar (lambda (seg)
            (format nil "~A/~A"
                    (first (senn.im.converting:segment-forms seg))
                    (senn.im.converting:segment-pron seg)))
          (senn.im.converting:state-segments state)))

(defmacro hachee-convert (&key test)
  `(let ((service (senn.fcitx.stateful-ime:make-service
                   :kkc (make-instance
                         'senn.im.kkc.hachee:kkc
                         :hachee-impl-lm-kkc
                         (senn.im.kkc.hachee:build-hachee-impl-lm-kkc)))))
     (let ((state (senn.im.converting:convert
                   (senn.fcitx.stateful-ime:service-ime service)
                   "とうきょうにいきました")))
       (,test (equal
               (senn.t.im-util:converting-state-segment-strings state)
               '("東京/とうきょう" "に/に" "行/い" "き/き" "ま/ま" "し/し" "た/た"))))))

(senn.t.fcitx:add-tests
 :hachee
 hachee-convert)
