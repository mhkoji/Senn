(defpackage :senn.t.scenario.fcitx.hachee
  (:use :cl))
(in-package :senn.t.scenario.fcitx.hachee)

(defun converting-state-segment-strings (state)
  (mapcar (lambda (seg)
            (format nil "~A/~A"
                    (first (senn.im.converting:segment-forms seg))
                    (senn.im.converting:segment-pron seg)))
          (senn.im.converting:state-segments state)))

(defmacro hachee-convert (&key test)
  `(let ((ime (senn.fcitx.stateful-ime-hachee:make-ime
               (senn.im.kkc.hachee:build-kkc))))
     (,test
      (equal
       (converting-state-segment-strings
        (senn.im.converting:convert ime "とうきょうにいきました"))
      '("東京/とうきょう" "に/に" "行/い" "き/き" "ま/ま" "し/し" "た/た")))))

(senn.t.scenario.fcitx:add-tests
 :hachee
 hachee-convert)