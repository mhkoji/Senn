(in-package :senn.fcitx.im)

(defmethod senn.fcitx.ime:select-candidate
    ((s t)
     (ime ime)
     (index t))
  (resp nil))

(defmethod senn.fcitx.ime:select-candidate
    ((s converting:state)
     (ime ime)
     (index integer))
  (converting:current-segment-candidates-set! s index)
  (resp (converting:converting-view s) :state s))

(defmethod senn.fcitx.ime:select-candidate
    ((s inputting:state)
     (ime ime)
     (index integer))
  (let ((predictions (inputting:state-predictions s)))
    (if (< -1 index (length predictions))
        (let* ((new-state (selecting-from-predictions:make-state
                           :predictions predictions
                           :current-index index))
               (view (selecting-from-predictions:editing-view new-state)))
          (resp view :state new-state))
        (resp nil))))
