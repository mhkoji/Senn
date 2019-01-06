(in-package :senn.fcitx.server)

(defun buffer-cursor-pos-in-utf-8 (buffer)
  (length (sb-ext:string-to-octets
           (subseq (senn.buffer:buffer-string buffer)
                   0
                   (senn.buffer:buffer-cursor-pos buffer))
           :external-format :utf-8)))

(defmethod make-response ((s editing) consumed)
  (format nil "~A ~A ~A ~A~%"
          :editing
          (if consumed 1 0)
          (buffer-cursor-pos-in-utf-8 (editing-buffer s))
          (senn.buffer:buffer-string (editing-buffer s))))


(defmethod make-response ((s converting) consumed)
  (declare  (ignore consumed))
  (format nil "~A ~A~%"
          :converting
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
                   (senn.segment:segment-current-index segment)))))))))



(defmethod make-response ((s committed) consumed)
  (declare  (ignore consumed))
  (format nil "~A ~A~%"
          :committed
          (committed-input s)))
