(defpackage :senn.win.history
  (:use :cl)
  (:export :get-history
           :get-form
           :convert-overwrite-mixin
           :convert-overwrite-mixin-set-holder))
(in-package :senn.win.history)

(defgeneric get-history (history-holder))

(defgeneric get-form (history pron))

(defun apply-history (history segs)
  (mapcar (lambda (seg)
            (let* ((pron (senn.im.kkc:segment-pron seg))
                   (history-form (get-form history pron)))
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

;;;

(defclass convert-overwrite-mixin ()
  ((holder
    :initarg :holder
    :initform nil
    :accessor convert-overwrite-mixin-holder)))

(defmethod senn.im.kkc:convert ((mixin convert-overwrite-mixin) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (let ((segs (call-next-method)))
    (let ((holder (convert-overwrite-mixin-holder mixin)))
      (or (when holder
            (apply-history (get-history holder) segs))
          segs))))

(defgeneric convert-overwrite-mixin-set-holder (mixin holder)
  (:method (mixin fn)
    nil)
  (:method ((mixin convert-overwrite-mixin) holder)
    (setf (convert-overwrite-mixin-holder mixin) holder)))
