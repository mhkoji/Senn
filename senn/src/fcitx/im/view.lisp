(defpackage :senn.fcitx.im.view
  (:use :cl)
  (:export :editing-by-string
           :editing-by-buffer
           :converting-cursor-form
           :converting))
(in-package :senn.fcitx.im.view)

(defun length-utf8 (string)
  (length (babel:string-to-octets string :encoding :utf-8)))

(defun buffer-cursor-pos-utf8 (buffer)
  (let ((string (senn.im.buffer:buffer-string buffer))
        (cursor-pos (senn.im.buffer:buffer-cursor-pos buffer)))
    (length-utf8 (subseq string 0 cursor-pos))))

(defun editing-view (cursor-pos
                     input
                     predictions
                     prediction-index
                     committed-string)
  (let ((json (jsown:new-js
                ("cursor-pos"       cursor-pos)
                ("input"            input)
                ("predictions"      predictions)
                ("prediction-index" (or prediction-index -1))
                ("committed-input"  committed-string))))
    (format nil "EDITING ~A" (jsown:to-json json))))

(defun editing-by-string (string predictions prediction-index)
  (editing-view (length-utf8 string) string predictions prediction-index ""))

(defun editing-by-buffer (buffer predictions prediction-index committed-string)
  (editing-view (buffer-cursor-pos-utf8 buffer)
                (senn.im.buffer:buffer-string buffer)
                predictions
                prediction-index
                committed-string))

(defun converting-cursor-form (candidates candidate-index)
  (jsown:new-js
   ("candidates"      candidates)
   ("candidate-index" candidate-index)))

(defun converting (forms cursor-form-index cursor-form)
  (let ((json (jsown:new-js
               ("forms" forms)
               ("cursor-form-index" cursor-form-index)
               ("cursor-form" cursor-form))))
    (format nil "CONVERTING ~A" (jsown:to-json json))))
