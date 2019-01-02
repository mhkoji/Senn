(defpackage :hachee.input-method.fcitx.states
  (:use :cl)
  (:export :editing
           :make-editing
           :editing-buffer

           :converting
           :make-converting
           :converting-segments
           :converting-current-segment
           :converting-current-input
           :converting-current-segment-index
           :converting-move-curret-segment

           :segment-current-form

           :committed
           :make-committed
           :committed-input)
  (:import-from :alexandria
                :if-let))
(in-package :hachee.input-method.fcitx.states)

(defstruct editing
  (buffer (hachee.input-method.buffer:make-buffer)))


(defstruct converting
  segments
  pronunciation
  (current-segment-index 0))

(defun converting-move-curret-segment (c diff)
  (let ((new-index (+ (converting-current-segment-index c) diff)))
    (when (<= 0 new-index (1- (length (converting-segments c))))
      (setf (converting-current-segment-index c) new-index)))
  c)

(defun converting-current-segment (c)
  (elt (converting-segments c)
       (converting-current-segment-index c)))

(defun converting-current-input (c)
  (format nil "~{~A~}"
          (mapcar #'hachee.input-method.segment:segment-current-form
                  (converting-segments c))))


(defstruct committed input)
