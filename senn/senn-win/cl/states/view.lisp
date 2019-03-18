(in-package :senn.win.states)

(defmethod to-view ((s editing))
  (senn.buffer:buffer-string (editing-buffer s)))

(defmethod to-view ((s committed))
  (committed-input s))
