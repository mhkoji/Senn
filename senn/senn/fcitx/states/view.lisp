(in-package :senn.fcitx.states)

(defun buffer-cursor-pos-in-utf-8 (buffer)
  (length (sb-ext:string-to-octets
           (subseq (senn.buffer:buffer-string buffer)
                   0
                   (senn.buffer:buffer-cursor-pos buffer))
           :external-format :utf-8)))

(defmethod to-view ((s editing))
  (format nil "~A ~A"
          (buffer-cursor-pos-in-utf-8 (editing-buffer s))
          (senn.buffer:buffer-string (editing-buffer s))))


(defmethod to-view ((s converting))
  (jsown:to-json
   (jsown:new-js
     ("forms"
      (mapcar #'senn.segment:segment-current-form
              (converting-segments s)))
     ("cursor-form-index"
      (converting-current-segment-index s))
     ("cursor-form"
      (let ((segment (converting-current-segment s)))
        (jsown:new-js
          ("candidates"
           (if (senn.segment:segment-has-more-forms-p segment)
               nil
               (senn.segment:segment-forms segment)))
          ("candidate-index"
           (senn.segment:segment-current-index segment))))))))


(defmethod to-view ((s committed))
  (committed-input s))
