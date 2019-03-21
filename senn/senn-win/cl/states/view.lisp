(in-package :senn.win.states)

(defmethod to-view ((s editing))
  (senn.buffer:buffer-string (editing-buffer s)))


(defmethod to-view ((s converting))
  (jsown:to-json
   (jsown:new-js
     ("forms"
      (mapcar #'senn.segment:segment-current-form
              (converting-segments s))))))


(defmethod to-view ((s committed))
  (jsown:to-json
   (jsown:new-js
     ("input" (committed-input s)))))
