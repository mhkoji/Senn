(in-package :hachee.input-method.fcitx.states)

(defun editing-insert-char (editing char)
  (destructuring-bind (new-buffer new-pos)
      (hachee.input-method.buffer:insert-char (editing-buffer editing)
                                              (editing-cursor-pos editing)
                                              char)
    (setf (editing-buffer editing) new-buffer)
    (setf (editing-cursor-pos editing) new-pos))
  editing)

(defun editing-try-move-cursor-pos (editing diff)
  (let ((new-pos (hachee.input-method.buffer:try-move-cursor-pos
                  (editing-buffer editing)
                  (editing-cursor-pos editing)
                  diff)))
    (when new-pos
      (setf (editing-cursor-pos editing) new-pos)))
  editing)
