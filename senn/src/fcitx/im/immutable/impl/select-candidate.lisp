(in-package :senn.fcitx.im.immutable.impl)

(defmethod senn.fcitx.im.immutable:select-candidate
    ((s t)
     (index t)
     (ime ime-mixin))
  (resp nil))

(defmethod senn.fcitx.im.immutable:select-candidate
    ((s converting:state)
     (index integer)
     (ime ime-mixin))
  (converting:current-segment-candidates-set! s index)
  (resp (converting:converting-view s) :state s))

(defmethod senn.fcitx.im.immutable:select-candidate
    ((s inputting:state)
     (index integer)
     (ime ime-mixin))
  (let ((predictions (inputting:state-predictions s)))
    (if (< -1 index (length predictions))
        (let* ((new-state (selecting-from-predictions:make-state
                           :predictions predictions
                           :current-index index))
               (view (selecting-from-predictions:editing-view new-state)))
          (resp view :state new-state))
        (resp nil))))
