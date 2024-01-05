(defpackage :senn.fcitx.im.immutable.select-candidate
  (:use :cl)
  (:import-from :senn.fcitx.im.immutable
                :select-candidate
                :resp)
  (:local-nicknames (:inputting :senn.fcitx.im.state.inputting)
                    (:converting :senn.fcitx.im.state.converting)
                    (:selecting-from-predictions
                     :senn.fcitx.im.state.selecting-from-predictions)))
(in-package :senn.fcitx.im.immutable.select-candidate)

(defmethod select-candidate ((s t) index)
  (resp nil))

(defmethod select-candidate ((s converting:state)
                             (index integer))
  (converting:current-segment-candidates-set! s index)
  (resp (converting:converting-view s) :state s))

(defmethod select-candidate ((s inputting:state)
                             (index integer))
  (let ((predictions (inputting:state-predictions s)))
    (if (< -1 index (length predictions))
        (let* ((new-state (selecting-from-predictions:make-state
                           :predictions predictions
                           :current-index index))
               (view (selecting-from-predictions:editing-view new-state)))
          (resp view :state new-state))
        (resp nil))))
