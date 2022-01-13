(defpackage :senn.fcitx.im.select-candidate
  (:use :cl :senn.fcitx.im)
  (:export :execute))
(in-package :senn.fcitx.im.select-candidate)

(defgeneric execute (state index))

(defun resp (consumed-p view &key state)
  (list consumed-p view :state state))

(defmethod execute ((s t) index)
  (resp nil nil))

(defmethod execute ((s senn.im.converting:state)
                    (index integer))
  (senn.im.converting:current-segment-candidates-set! s index)
  (resp t (converting-view/converting-state s) :state s))

(defmethod execute ((s senn.im.inputing:state)
                    (index integer))
  (let ((predictions (inputting-predictions s)))
    (if (< -1 index (length predictions))
        (let* ((new-state (make-selecting-from-predictions
                           :predictions predictions
                           :current-index index))
               (view (editing-view/selecting-from-predictions new-state)))
          (resp t view :state new-state))
        (resp nil nil))))
