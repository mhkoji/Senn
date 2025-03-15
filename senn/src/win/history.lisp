(defpackage :senn.win.history
  (:use :cl)
  (:export :make-history
           :history-put
           :history-apply))
(in-package :senn.win.history)

(defstruct history
  (hash (make-hash-table :test #'equal)))

(defun history-put (history pron form)
  (setf (gethash pron (history-hash history)) form))

(defun history-get-form (history pron)
  (gethash pron (history-hash history)))

(defun history-apply (history segs)
  (mapcar (lambda (seg)
            (let* ((pron (senn.im.kkc:segment-pron seg))
                   (history-form (history-get-form history pron)))
              (if (not history-form)
                  seg
                  (senn.im.kkc:make-segment
                   :pron pron
                   :candidates
                   (cons (senn.im.kkc:make-candidate :form history-form)
                         (remove history-form
                                 (senn.im.kkc:segment-candidates seg)
                                 :key #'senn.im.kkc:candidate-form
                                 :test #'string=))))))
          segs))
