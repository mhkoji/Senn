(defpackage :senn.fcitx.im
  (:use :cl)
  (:export :inputting
           :make-inputting
           :inputting-buffer
           :inputting-predictions

           :katakana
           :make-katakana
           :katakana-input

           :selecting-from-predictions
           :make-selecting-from-predictions
           :selecting-from-predictions-predictions
           :selecting-from-predictions-current-index
           :selecting-from-predictions-current-input
           :selecting-from-predictions-move-prediction

           :converting
           :make-converting
           :converting-pronunciation
           :converting-segments
           :converting-current-segment
           :converting-current-input
           :converting-current-segment-index
           :converting-move-curret-segment

           :editing-view/inputing-state
           :editing-view/katakana-state
           :editing-view/selecting-from-predictions
           :converting-view/converting-state))
(in-package :senn.fcitx.im)

(defstruct inputting
  (buffer (senn.im.buffer:make-buffer))
  predictions)


(defstruct (katakana (:constructor %make-katakana))
  (input ""))

(defun make-katakana (&key input)
  (%make-katakana :input (senn.ja:hiragana->katakana input)))


(defstruct selecting-from-predictions predictions current-index)

(defun selecting-from-predictions-current-input (s)
  (nth (selecting-from-predictions-current-index s)
       (selecting-from-predictions-predictions s)))

(defun selecting-from-predictions-move-prediction (s diff)
  (let ((new-index (+ (selecting-from-predictions-current-index s) diff)))
    (when (<= 0 new-index
              (1- (length (selecting-from-predictions-predictions s))))
      (setf (selecting-from-predictions-current-index s) new-index)))
  s)


(defstruct converting
  segments
  pronunciation
  (current-segment-index 0))

(defun converting-move-curret-segment (c diff)
  (let ((new-index (+ (converting-current-segment-index c) diff)))
    (when (<= 0 new-index (1- (length (converting-segments c))))
      (setf (converting-current-segment-index c) new-index)))
  c)

(defun converting-current-segment (c)
  (elt (converting-segments c)
       (converting-current-segment-index c)))

(defun converting-current-input (c)
  (format nil "~{~A~}"
          (mapcar #'senn.im.segment:segment-current-form
                  (converting-segments c))))

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

(defun editing-view/inputing-state (s &key committed-string)
  (let ((buffer (inputting-buffer s)))
    (make-editing-view (buffer-cursor-pos-utf8 buffer)
                       (senn.im.buffer:buffer-string buffer)
                       (inputting-predictions s)
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
            (mapcar #'senn.im.segment:segment-current-form
                    (converting-segments s)))
           ("cursor-form-index"
            (converting-current-segment-index s))
           ("cursor-form"
            (let ((segment (converting-current-segment s)))
              (if (senn.im.segment:segment-shows-katakana-p segment)
                  (jsown:new-js
                    ("candidates"      nil)
                    ("candidate-index" -1))
                  (jsown:new-js
                    ("candidates"
                     (if (senn.im.segment:segment-has-more-candidates-p
                          segment)
                         nil
                         (senn.im.segment:segment-forms segment)))
                    ("candidate-index"
                     (senn.im.segment:segment-current-index segment)))))))))
    (format nil "CONVERTING ~A" (jsown:to-json json))))
