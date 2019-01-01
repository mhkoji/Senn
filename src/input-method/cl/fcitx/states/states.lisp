(defpackage :hachee.input-method.fcitx.states
  (:use :cl)
  (:export :editing
           :make-editing
           :editing-buffer
           :editing-cursor-pos
           :editing-try-move-cursor-pos
           :editing-insert-char

           :converting
           :make-converting
           :converting-segments
           :converting-current-segment
           :make-segment
           :segment-pron
           :segment-has-more-forms-p
           :segment-append-forms!
           :segment-try-move-cursor-pos!

           :segment-current-form

           :committed
           :make-committed
           :committed-input)
  (:import-from :alexandria
                :if-let))
(in-package :hachee.input-method.fcitx.states)

(defstruct editing
  (buffer "")
  (cursor-pos 0))

(defun editing-try-move-cursor-pos (editing diff)
  (let ((new-pos
          (hachee.input-method.fcitx.states.editing:buffer-try-move-cursor-pos
           (editing-buffer editing)
           (editing-cursor-pos editing)
           diff)))
    (when new-pos
      (setf (editing-cursor-pos editing) new-pos)))
  editing)

(defun editing-insert-char (editing char)
  (destructuring-bind (new-buffer new-pos)
      (hachee.input-method.fcitx.states.editing:buffer-insert-char
       (editing-buffer editing)
       (editing-cursor-pos editing)
       char)
    (setf (editing-buffer editing) new-buffer)
    (setf (editing-cursor-pos editing) new-pos))
  editing)


(defstruct segment
  pron
  forms
  current-index
  has-more-forms-p)

(defun segment-append-forms! (segment forms)
  (let* ((in-forms (intersection (segment-forms segment) forms
                                 :test #'string=))
         (new-forms (set-difference forms in-forms
                                    :test #'string=)))
    (setf (segment-forms segment)
          (append (segment-forms segment) new-forms))
    (setf (segment-has-more-forms-p segment) nil))
  segment)


(defun segment-try-move-cursor-pos! (segment diff)
  (let ((new-index (+ (segment-current-index segment) diff)))
    (when (<= 0 new-index (1- (length (segment-forms segment))))
      (setf (segment-current-index segment) new-index)))
  segment)


(defun segment-current-form (s)
  (elt (segment-forms s) (segment-current-index s)))


(defstruct converting
  segments
  pronunciation
  (current-segment-index 0))

(defun converting-current-segment (c)
  (elt (converting-segments c)
       (converting-current-segment-index c)))


(defstruct committed input)
