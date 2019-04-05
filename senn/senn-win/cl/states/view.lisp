(in-package :senn.win.states)

(defmethod to-view ((s editing))
  (senn.buffer:buffer-string (editing-buffer s)))


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
  (jsown:to-json
   (jsown:new-js
     ("input" (committed-input s)))))
