(defpackage :hachee.kkc.eval
  (:use :cl)
  (:import-from :hachee.algorithm.longest-common-subsequence
                :compute-length)
  (:export :eval-by-lcs
           :convert-save-eval))
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

(defun eval-from-file (gold-list converted-file)
  (let ((sys-list (with-open-file (in converted-file)
                    (loop for line = (read-line in nil nil)
                          while line collect line))))
    (eval-by-lcs (mapcar #'list gold-list sys-list))))

(defun convert-save (kkc pron-list out-file)
  (ensure-directories-exist out-file)
  (with-open-file (out out-file :direction :output)
    (loop for pron in pron-list while pron do
      (let ((entries (hachee.kkc:convert kkc pron)))
        (format out "~A~%"
                (apply #'concatenate 'string
                       (mapcar (lambda (e)
                                 (hachee.kkc.dictionary:unit-form
                                  (hachee.kkc.convert:entry-unit e)))
                               entries)))))))

(defun convert-save-eval (kkc pathname dir)
  (labels ((sentence-pron (s)
             (apply #'concatenate
                    'string
                    (mapcar #'hachee.kkc.dictionary:unit-pron
                            (hachee.kkc.build.file:sentence-units s))))
           (sentence-form (s)
             (apply #'concatenate
                    'string
                    (mapcar #'hachee.kkc.dictionary:unit-form
                            (hachee.kkc.build.file:sentence-units s)))))

    (let ((sentences (hachee.kkc.build.file:file->sentences pathname))
          (out-file (format nil "~A/converted.txt" dir)))
      (let ((pron-list (mapcar #'sentence-pron sentences)))
        (convert-save kkc pron-list out-file))
      (let ((gold-list (mapcar #'sentence-form sentences)))
        (eval-from-file gold-list out-file)))))
