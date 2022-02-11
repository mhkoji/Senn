(defpackage :senn.fcitx.im
  (:use :cl)
  (:export :ime
           :ime-max-candidate-count
           :ime-kkc
           :ime-predictor
           :katakana
           :katakana-input
           :make-katakana
           :selecting-from-predictions
           :selecting-from-predictions-predictions
           :selecting-from-predictions-current-index
           :selecting-from-predictions-current-input
           :selecting-from-predictions-move!
           :make-selecting-from-predictions
           :editing-view/inputting-state
           :editing-view/katakana-state
           :editing-view/selecting-from-predictions
           :converting-view/converting-state))
(in-package :senn.fcitx.im)

(defclass ime () ())
(defgeneric ime-max-candidate-count (ime)
  (:method ((ime ime))
    nil))
(defgeneric ime-kkc (ime))
(defgeneric ime-predictor (ime)
  (:method ((ime ime))
    nil))


(defstruct (katakana (:constructor %make-katakana))
  (input ""))

(defun make-katakana (&key input)
  (%make-katakana :input (senn.ja:hiragana->katakana input)))


(defstruct selecting-from-predictions predictions current-index)

(defun selecting-from-predictions-current-input (s)
  (nth (selecting-from-predictions-current-index s)
       (selecting-from-predictions-predictions s)))

(defun selecting-from-predictions-move! (s diff)
  (let ((new-index (+ (selecting-from-predictions-current-index s) diff)))
    (when (<= 0 new-index
              (1- (length (selecting-from-predictions-predictions s))))
      (setf (selecting-from-predictions-current-index s) new-index)))
  s)


;;; Views

(defun length-utf8 (string)
  (length (babel:string-to-octets string :encoding :utf-8)))

(defun buffer-cursor-pos-utf8 (buffer)
  (let ((string (senn.im.buffer:buffer-string buffer))
        (cursor-pos (senn.im.buffer:buffer-cursor-pos buffer)))
    (length-utf8 (subseq string 0 cursor-pos))))

(defun make-editing-view (cursor-pos
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

(defun editing-view/inputting-state (s &key committed-string)
  (let ((buffer (senn.im.inputting:state-buffer s)))
    (make-editing-view (buffer-cursor-pos-utf8 buffer)
                       (senn.im.buffer:buffer-string buffer)
                       (senn.im.inputting:state-predictions s)
                       nil
                       (or committed-string ""))))

(defun editing-view/katakana-state (s)
  (let ((katakana-input (katakana-input s)))
    (make-editing-view (length-utf8 katakana-input)
                       katakana-input nil nil "")))

(defun editing-view/selecting-from-predictions (s)
  (let ((input (selecting-from-predictions-current-input s)))
    (make-editing-view (length-utf8 input)
                       input
                       (selecting-from-predictions-predictions s)
                       (selecting-from-predictions-current-index s)
                       "")))

(defun converting-view/converting-state (s)
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
