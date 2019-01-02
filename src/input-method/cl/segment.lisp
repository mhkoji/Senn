(defpackage :hachee.input-method.segment
  (:use :cl)
  (:export :make-segment
           :segment-current-form
           :append-forms!
           :try-move-cursor-pos!))
(in-package :hachee.input-method.segment)

(defstruct segment
  pron
  forms
  current-index
  has-more-forms-p)


(defun segment-current-form (s)
  (elt (segment-forms s) (segment-current-index s)))


(defun append-forms! (segment get-forms)
  (when (segment-has-more-forms-p segment)
    (let* ((all-forms (funcall get-forms (segment-pron segment)))
           (new-forms (set-difference all-forms
                                      (segment-forms segment)
                                      :test #'string=)))
      (setf (segment-forms segment)
            (append (segment-forms segment) new-forms))
      (setf (segment-has-more-forms-p segment)
            nil)))
  segment)


(defun try-move-cursor-pos! (segment diff)
  (let ((new-index (+ (segment-current-index segment) diff)))
    (when (<= 0 new-index (1- (length (segment-forms segment))))
      (setf (segment-current-index segment) new-index)))
  segment)
