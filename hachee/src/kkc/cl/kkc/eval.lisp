(defpackage :hachee.kkc.eval
  (:use :cl)
  (:import-from :hachee.algorithm.longest-common-subsequence
                :compute-length)
  (:export :eval-by-lcs))
(in-package :hachee.kkc.eval)

(defun eval-by-lcs (gold-sys-list)
  (let ((len-gold 0)
        (len-lcs  0)
        (len-sys  0))
    (loop for (gold-str sys-str) in gold-sys-list
          do (progn
               (incf len-gold (length gold-str))
               (incf len-sys (length sys-str))
               (incf len-lcs (compute-length gold-str sys-str))))
    (let* ((precision
            (* 1.0d0 (/ len-lcs len-sys)))
           (recall
            (* 1.0d0 (/ len-lcs len-gold)))
           (f-measure
            (/ (* recall precision 2)
               (+ recall precision))))
      (values f-measure precision recall))))
