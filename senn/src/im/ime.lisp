(defpackage :senn.im.ime
  (:use :cl)
  (:export :ime
           :ime-kkc
           :ime-predictor
           :convert
           :predict
           :segment-append-candidates!))
(in-package :senn.im.ime)

(defclass ime () ())

(defgeneric ime-kkc (ime))

(defgeneric ime-predictor (ime)
  (:method ((ime ime))
    nil))

(defun convert (ime pron &key 1st-boundary-index)
  (senn.im.kkc:convert (ime-kkc ime) pron
                       :1st-boundary-index 1st-boundary-index))

(defun lookup (ime pron &key prev next)
  (senn.im.kkc:lookup (ime-kkc ime) pron :prev prev :next next))

(defun predict (ime string)
  (let ((predictor (ime-predictor ime)))
    (when predictor
      (senn.im.predict:execute predictor string))))


(defun append-candidates (ime current-candidates pron)
  (labels ((exists-in-current-candidates-p (cand)
             (find-if (lambda (c)
                        (string= (senn.im.segment:candidate-form c)
                                 (senn.im.segment:candidate-form cand)))
                      current-candidates)))
    ;; Don't change the sort order of current-candidates.
    (append current-candidates
            (remove-if #'exists-in-current-candidates-p
                       (lookup ime pron)))))

(defun segment-append-candidates! (segment ime)
  (when (senn.im.segment:segment-has-more-candidates-p segment)
    (let ((pron (senn.im.segment:segment-pron segment))
          (current-candidates (senn.im.segment:segment-candidates segment)))
      (setf (senn.im.segment:segment-candidates segment)
            (append-candidates ime current-candidates pron))
      (setf (senn.im.segment:segment-has-more-candidates-p segment)
            nil)))
  segment)
