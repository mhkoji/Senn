(in-package :hachee.input-method.fcitx.controller)

(defmethod transit-by-input ((c controller)
                             (s committed)
                             code)
  (transit-by-input c (make-editing) code))


(defmethod transit-by-input ((c controller)
                             (s converting)
                             code)
  (make-committed
   :input (format nil "~{~A~}"
                  (mapcar #'segment-current-form
                          (converting-segments s)))))


(defmethod transit-by-input ((c controller)
                             (s editing)
                             code)
  (case code
    (32    ;; Space key
     (let ((pronunciation (editing-buffer s)))
       (let ((words (hachee.kkc:convert (controller-kkc c)
                                        pronunciation)))
         (make-converting
          :segments (mapcar (lambda (w)
                              (make-segment
                               :pron (hachee.kkc:word-pron w)
                               :forms (list (hachee.kkc:word-form w))
                               :current-index 0))
                            words)
          :pronunciation pronunciation))))
    (65293 ;; Enter key
     (make-committed :input (editing-buffer s)))
    (65361 ;; Left key
     (when (< 0 (editing-cursor-pos s))
       (decf (editing-cursor-pos s)))
     s)
    (65363 ;; Right key
     (when (< (editing-cursor-pos s) (length (editing-buffer s)))
       (incf (editing-cursor-pos s)))
     s)
    (t
     (destructuring-bind (new-buffer new-pos)
         (hachee.input-method.fcitx.states.editing:buffer-insert-char
          (editing-buffer s)
          (editing-cursor-pos s)
          (code-char code))
       (setf (editing-buffer s) new-buffer)
       (setf (editing-cursor-pos s) new-pos))
     s)))
