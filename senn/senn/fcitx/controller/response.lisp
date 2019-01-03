(in-package :senn.fcitx.controller)

(defun buffer-cursor-pos-in-utf-8 (buffer)
  (length (sb-ext:string-to-octets
           (subseq (senn.buffer:buffer-string buffer)
                   0
                   (senn.buffer:buffer-cursor-pos buffer))
           :external-format :utf-8)))

(defmethod make-response ((s editing))
  (format nil "~A ~A ~A~%"
          :editing
          (buffer-cursor-pos-in-utf-8 (editing-buffer s))
          (senn.buffer:buffer-string (editing-buffer s))))


(defmethod make-response ((s converting))
  (let ((forms (format nil "~{~A~,^ ~}"
                       (mapcar #'senn.segment:segment-current-form
                               (converting-segments s)))))
    (format nil "~A ~A ~A~%"
            :converting
            (converting-current-segment-index s)
            forms)))


(defmethod make-response ((s committed))
  (let ((input (committed-input s)))
    (format nil "~A ~A ~A~%"
            :committed
            (length (sb-ext:string-to-octets
                     input
                     :external-format :utf-8))
            input)))
