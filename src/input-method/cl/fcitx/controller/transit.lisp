(in-package :hachee.input-method.fcitx.controller)

(defmethod transit-by-input ((c controller)
                             (s committed)
                             code)
  (transit-by-input c (make-editing) code))


(defmethod transit-by-input ((c controller)
                             (s converting)
                             code)
  (case code
    (32    ;; Space key
     (let ((segment (converting-current-segment s)))
       (when (segment-has-more-forms-p segment)
         (let ((words (hachee.kkc:lookup (controller-kkc c)
                                         (segment-pron segment))))
           (segment-append-forms!
            segment
            (mapcar #'hachee.kkc:word-form words))))
       (segment-try-move-cursor-pos! segment +1))
     s)
    (t
     (make-committed
      :input (format nil "~{~A~}"
                     (mapcar #'segment-current-form
                             (converting-segments s)))))))


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
                               :has-more-forms-p t
                               :current-index 0))
                            words)
          :pronunciation pronunciation))))
    (65293 ;; Enter key
     (make-committed :input (editing-buffer s)))
    (65361 ;; Left key
     (editing-try-move-cursor-pos s -1))
    (65363 ;; Right key
     (editing-try-move-cursor-pos s +1))
    (t
     (editing-insert-char s (code-char code)))))
