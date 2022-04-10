(defpackage :senn.fcitx.im.view
  (:use :cl :senn.fcitx.im.state)
  (:export :editing/inputting-state
           :editing/katakana-state
           :editing/selecting-from-predictions-state
           :converting/converting-state))
(in-package :senn.fcitx.im.view)

(defun length-utf8 (string)
  (length (babel:string-to-octets string :encoding :utf-8)))

(defun buffer-cursor-pos-utf8 (buffer)
  (let ((string (senn.im.buffer:buffer-string buffer))
        (cursor-pos (senn.im.buffer:buffer-cursor-pos buffer)))
    (length-utf8 (subseq string 0 cursor-pos))))

(defun editing (cursor-pos
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

(defun editing/inputting-state (s &key committed-string)
  (let ((buffer (senn.im.inputting:state-buffer s)))
    (editing (buffer-cursor-pos-utf8 buffer)
             (senn.im.buffer:buffer-string buffer)
             (senn.im.inputting:state-predictions s)
             nil
             (or committed-string ""))))

(defun editing/katakana-state (s)
  (let ((katakana-input (katakana-input s)))
    (editing (length-utf8 katakana-input)
             katakana-input nil nil "")))

(defun editing/selecting-from-predictions-state (s)
  (let ((input (selecting-from-predictions-current-input s)))
    (editing (length-utf8 input)
             input
             (selecting-from-predictions-predictions s)
             (selecting-from-predictions-current-index s)
             "")))

(defun converting/converting-state (s)
  (let ((json
         (jsown:new-js
           ("forms"
            (mapcar #'senn.im.converting:segment-cursor-pos-form
                    (senn.im.converting:state-segments s)))
           ("cursor-form-index"
            (senn.im.converting:state-current-segment-index s))
           ("cursor-form"
            (let ((segment (senn.im.converting:current-segment s)))
              (if (senn.im.converting:segment-shows-katakana-p segment)
                  (jsown:new-js
                    ("candidates"      nil)
                    ("candidate-index" -1))
                  (jsown:new-js
                    ("candidates"
                     (if (senn.im.converting:segment-has-more-candidates-p
                          segment)
                         nil
                         (senn.im.converting:segment-forms segment)))
                    ("candidate-index"
                     (senn.im.converting:segment-current-index
                      segment)))))))))
    (format nil "CONVERTING ~A" (jsown:to-json json))))
