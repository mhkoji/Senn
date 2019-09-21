(defpackage :senn.im
  (:use :cl)
  (:export :make-ime
           :ime
           :convert
           :lookup-forms)
  (:import-from :senn.segment
                :make-segment))
(in-package :senn.im)

(defstruct ime kkc)

(defun convert (ime pron)
  (let ((words (senn.kkc:convert (ime-kkc ime) pron)))
    (mapcar (lambda (w)
              (make-segment :pron (senn.kkc:word-pron w)
                            :forms (list (senn.kkc:word-form w))
                            :has-more-forms-p t
                            :current-index 0))
            words)))

(defun lookup-forms (ime pron)
  (senn.kkc:lookup-forms (ime-kkc ime) pron))
