(in-package :senn.fcitx.im.impl)

(defmethod senn.fcitx.im:select-candidate
    ((s t)
     (ime ime-mixin)
     (index t))
  (resp nil))

(defmethod senn.fcitx.im:select-candidate
    ((s converting:state)
     (ime ime-mixin)
     (index integer))
  (converting:current-segment-candidates-set! s index)
  (resp (converting:converting-view s) :state s))

(defmethod senn.fcitx.im:select-candidate
    ((s inputting:state)
     (ime ime-mixin)
     (index integer))
  (let ((predictions (inputting:state-predictions s)))
    (if (< -1 index (length predictions))
        (let* ((new-state (selecting-from-predictions:make-state
                           :predictions predictions
                           :current-index index))
               (view (selecting-from-predictions:editing-view new-state)))
          (resp view :state new-state))
        (resp nil))))
