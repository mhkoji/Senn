(defpackage :senn.segment
  (:use :cl)
  (:export :make-segment
           :segment-forms
           :segment-has-more-forms-p
           :segment-current-form
           :segment-current-index
           :append-forms!
           :try-move-cursor-pos!))
(in-package :senn.segment)

(defstruct segment
  pron
  forms
  current-index
  has-more-forms-p)


(defun segment-current-form (s)
  (elt (segment-forms s) (segment-current-index s)))


(defun append-forms! (segment get-forms)
  (when (segment-has-more-forms-p segment)
    (let ((new-forms (remove-if (lambda (f)
                                  (member f (segment-forms segment)
                                          :test #'string=))
                                (funcall get-forms (segment-pron segment)))))
      (setf (segment-forms segment)
            (append (segment-forms segment) new-forms))
      (setf (segment-has-more-forms-p segment)
            nil)))
  segment)


(defun try-move-cursor-pos! (segment diff)
  (with-accessors ((forms segment-forms)
                   (current-index segment-current-index)) segment
    (setf current-index (mod (+ current-index diff)
                             (length forms))))
  segment)
