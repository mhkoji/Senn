(defpackage :senn.fcitx.im.view
  (:use :cl)
  (:export :editing
           :length-utf8
           :converting-cursor-form
           :converting))
(in-package :senn.fcitx.im.view)

(defun length-utf8 (string)
  (length (babel:string-to-octets string :encoding :utf-8)))

(defun editing (cursor-pos
                input
                predictions
                prediction-index
                committed-string)
  (let ((view
         (with-output-to-string (stream)
           (yason:with-output (stream)
             (yason:encode
              (alexandria:plist-hash-table
               (list "cursor-pos"       cursor-pos
                     "input"            input
                     "predictions"      (or predictions #())
                     "prediction-index" (or prediction-index -1)
                     "committed-input"  committed-string)
               :test #'equal)
              stream)))))
    (format nil "EDITING ~A" view)))

(defun converting-cursor-form (candidates candidate-index)
  (alexandria:plist-hash-table
   (list "candidates"      (or candidates #())
         "candidate-index" candidate-index)
   :test #'equal))

(defun converting (forms cursor-form-index cursor-form)
  (let ((view
         (with-output-to-string (stream)
           (yason:with-output (stream)
             (yason:encode
              (alexandria:plist-hash-table
               (list "forms" (or forms #())
                     "cursor-form-index" cursor-form-index
                     "cursor-form" cursor-form)
               :test #'equal)
              stream)))))
    (format nil "CONVERTING ~A" view)))
