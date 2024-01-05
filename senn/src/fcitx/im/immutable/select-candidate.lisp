(defpackage :senn.fcitx.im.immutable.select-candidate
  (:use :cl :senn.fcitx.im.view)
  (:export :execute))
(in-package :senn.fcitx.im.immutable.select-candidate)

(defgeneric execute (state index))

(defun resp (consumed-p view &key state)
  (list (list consumed-p view) state))

(defmethod execute ((s t) index)
  (resp nil nil))

(defmethod execute ((s senn.fcitx.im.state.converting:state)
                    (index integer))
  (senn.fcitx.im.state.converting:current-segment-candidates-set! s index)
  (resp t (senn.fcitx.im.state.converting:converting-view s)
        :state s))

(defmethod execute ((s senn.fcitx.im.state.inputting:state)
                    (index integer))
  (let ((predictions (senn.fcitx.im.state.inputting:state-predictions s)))
    (if (< -1 index (length predictions))
        (let* ((new-state
                (senn.fcitx.im.state.selecting-from-predictions:make-state
                 :predictions predictions
                 :current-index index))
               (view
                (senn.fcitx.im.state.selecting-from-predictions:editing-view
                 new-state)))
          (resp t view :state new-state))
        (resp nil nil))))
