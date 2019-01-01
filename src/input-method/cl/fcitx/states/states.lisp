(defpackage :hachee.input-method.fcitx.states
  (:use :cl)
  (:export :editing
           :make-editing
           :editing-buffer
           :editing-cursor-pos

           :converting
           :make-segment
           :make-converting
           :segment-current-form
           :converting-segments

           :committed
           :make-committed
           :committed-input)
  (:import-from :alexandria
                :if-let))
(in-package :hachee.input-method.fcitx.states)

(defstruct editing
  (buffer "")
  (cursor-pos 0))


(defstruct segment
  pron forms current-index)

(defun segment-current-form (s)
  (elt (segment-forms s) (segment-current-index s)))

(defstruct converting segments pronunciation)


(defstruct committed input)
