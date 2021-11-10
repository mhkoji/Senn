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
  (:method-combination append)
  (:method append ((ime t) (string t))
    ;; returns no predictions
    nil))

(defun append-candidates (ime segment)
  (when (senn.segment:segment-has-more-candidates-p segment)
    (let ((current-candidates
           (senn.segment:segment-candidates segment))
          (new-candidates
           (lookup ime (senn.segment:segment-pron segment))))
      (setf (senn.segment:segment-candidates segment)
            (append current-candidates
                    (remove-if (lambda (cand)
                                 (member (senn.segment:candidate-form cand)
                                         current-candidates
                                         :key #'senn.segment:candidate-form
                                         :test #'string=))
                               new-candidates)))
      (setf (senn.segment:segment-has-more-candidates-p segment)
            nil)))
  segment)
