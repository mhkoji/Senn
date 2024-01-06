(defpackage :senn.fcitx.im.immutable.select-candidate
  (:use :cl)
  (:export :execute)
  (:import-from :senn.fcitx.im.immutable
                :resp)
  (:local-nicknames (:inputting :senn.fcitx.im.state.inputting)
                    (:converting :senn.fcitx.im.state.converting)
                    (:selecting-from-predictions
                     :senn.fcitx.im.state.selecting-from-predictions)))
(in-package :senn.fcitx.im.immutable.select-candidate)

(defgeneric execute (state index))

(defmethod execute ((s t) index)
  (resp nil))

(defmethod execute ((s converting:state) (index integer))
  (converting:current-segment-candidates-set! s index)
  (resp (converting:converting-view s) :state s))

(defmethod execute ((s inputting:state) (index integer))
  (let ((predictions (inputting:state-predictions s)))
    (if (< -1 index (length predictions))
        (let* ((new-state (selecting-from-predictions:make-state
                           :predictions predictions
                           :current-index index))
               (view (selecting-from-predictions:editing-view new-state)))
          (resp view :state new-state))
        (resp nil))))
