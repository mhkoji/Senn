(defpackage :senn.t.fcitx-util
  (:use :cl)
  (:export :converting-state-segment-strings))
(in-package :senn.t.fcitx-util)
  
(defun converting-state-segment-strings (state)
  (mapcar (lambda (seg)
            (format nil "~A/~A"
                    (first (senn.im.converting:segment-forms seg))
                    (senn.im.converting:segment-pron seg)))
          (senn.im.converting:state-segments state)))
