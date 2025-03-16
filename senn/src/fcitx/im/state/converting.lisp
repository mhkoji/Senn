(defpackage :senn.fcitx.im.state.converting
  (:use :cl :senn.im.converting)
  (:export :mixin
           :ime-kkc
           :ime-max-candidate-count

           :state
           :state-pronunciation
           :current-segment-move!
           :current-segment-katakana!
           :current-segment-candidates-move!
           :current-segment-candidates-set!
           :current-input
           :convert

           :converting-view))
(in-package :senn.fcitx.im.state.converting)

(defun converting-view (s)
  (senn.fcitx.view:converting
   (mapcar #'segment-cursor-pos-form (state-segments s))
   (state-current-segment-index s)
   (let ((segment (current-segment s)))
     (if (segment-shows-katakana-p segment)
         (senn.fcitx.view:converting-cursor-form
          nil
          -1)
         (senn.fcitx.view:converting-cursor-form
          (if (segment-has-more-candidates-p segment)
              nil
              (segment-forms segment))
          (segment-current-index segment))))))
