(defpackage :senn.im.ime
  (:use :cl)
  (:export :ime
           :convert
           :lookup
           :predict
           :segment-append-candidates!))
(in-package :senn.im.ime)

(defclass ime () ())

(defgeneric convert (ime pron &key 1st-boundary-index))

(defgeneric lookup (ime pron &key prev next))

(defgeneric predict (ime string)
  (:method-combination append)
  (:method append ((ime t) (string t))
    ;; returns no predictions
    nil))

(defun append-candidates (ime current-candidates pron)
  (remove-duplicates (append current-candidates (lookup ime pron))
                     :key #'senn.im.segment:candidate-form
                     :test #'string=))

(defun segment-append-candidates! (ime segment)
  (when (senn.im.segment:segment-has-more-candidates-p segment)
    (let ((pron (senn.im.segment:segment-pron segment))
          (current-candidates (senn.im.segment:segment-candidates segment)))
      (setf (senn.im.segment:segment-candidates segment)
            (append-candidates ime current-candidates pron))
      (setf (senn.im.segment:segment-has-more-candidates-p segment)
            nil)))
  segment)
