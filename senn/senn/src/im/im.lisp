(defpackage :senn.im
  (:use :cl)
  (:export :ime
           :convert
           :lookup
           :predict
           :append-candidates))
(in-package :senn.im)

(defclass ime () ())

(defgeneric convert (ime pron &key 1st-boundary-index))

(defgeneric lookup (ime pron &key prev next))

(defgeneric predict (ime string)
  (:method ((ime t) (string t))
    ;; returns no predictions
    nil))

(defun append-candidates (ime segment)
  (when (senn.segment:segment-has-more-candidates-p segment)
    (let ((new-candidates (lookup ime (senn.segment:segment-pron segment))))
      (setf (senn.segment:segment-candidates segment)
            (append (senn.segment:segment-candidates segment)
                    new-candidates))
      (setf (senn.segment:segment-has-more-candidates-p segment)
            nil)))
  segment)
