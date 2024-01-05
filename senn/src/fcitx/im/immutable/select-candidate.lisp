(defpackage :senn.fcitx.im.immutable.select-candidate
  (:use :cl)
  (:local-nicknames (:inputting :senn.fcitx.im.state.inputting)
                    (:converting :senn.fcitx.im.state.converting)
                    (:selecting-from-predictions
                     :senn.fcitx.im.state.selecting-from-predictions))
  (:export :execute))
(in-package :senn.fcitx.im.immutable.select-candidate)

(defgeneric execute (state index))

(defun resp (consumed-p view &key state)
  (list (list consumed-p view) state))

(defmethod execute ((s t) index)
  (resp nil nil))

(defmethod execute ((s converting:state)
                    (index integer))
  (converting:current-segment-candidates-set! s index)
  (resp t (converting:converting-view s) :state s))

(defmethod execute ((s inputting:state)
                    (index integer))
  (let ((predictions (inputting:state-predictions s)))
    (if (< -1 index (length predictions))
        (let* ((new-state (selecting-from-predictions:make-state
                           :predictions predictions
                           :current-index index))
               (view (selecting-from-predictions:editing-view new-state)))
          (resp t view :state new-state))
        (resp nil nil))))
