(in-package :senn.win.states)

(defmethod to-view ((s editing))
  (senn.buffer:buffer-string (editing-buffer s)))

(defmethod to-view ((s committed))
  (jsown:to-json
   (jsown:new-js
     ("input" (committed-input s)))))
