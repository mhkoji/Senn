(defpackage :senn.fcitx.im.select-candidate
  (:use :cl :senn.fcitx.im)
  (:export :execute))
(in-package :senn.fcitx.im.select-candidate)

(defgeneric execute (state index))

(defun resp (consumed-p view &key state)
  (list consumed-p view :state state))

(defmethod execute ((s t) index)
  (resp nil nil))

(defmethod execute ((s converting) (index integer))
  (let ((curr-seg (converting-current-segment s)))
    (senn.im.segment:try-set-cursor-pos! curr-seg index))
  (let ((view (converting-view/converting-state s)))
    (resp t view :state s)))

(defmethod execute ((s inputting) (index integer))
  (let ((predictions (inputting-predictions s)))
    (if (< -1 index (length predictions))
        (let* ((new-state (make-selecting-from-predictions
                           :predictions predictions
                           :current-index index))
               (view (editing-view/selecting-from-predictions new-state)))
          (resp t view :state new-state))
        (resp nil nil))))
