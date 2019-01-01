(in-package :hachee.input-method.fcitx.controller)

(defun editing-cursor-pos-in-utf-8 (editing)
  (length (sb-ext:string-to-octets
           (subseq (editing-buffer editing)
                   0
                   (editing-cursor-pos editing))
           :external-format :utf-8)))

(defmethod make-response ((s editing))
  (format nil "~A ~A ~A~%"
          :editing
          (editing-cursor-pos-in-utf-8 s)
          (editing-buffer s)))


(defmethod make-response ((s converting))
  (let ((input (format nil "~{~A~}"
                       (mapcar #'segment-current-form
                               (converting-segments s)))))
    (format nil "~A ~A ~A~%"
            :converting
            (length (sb-ext:string-to-octets
                     input
                     :external-format :utf-8))
            input)))

(defmethod make-response ((s committed))
  (let ((input (committed-input s)))
    (format nil "~A ~A ~A~%"
            :committed
            (length (sb-ext:string-to-octets
                     input
                     :external-format :utf-8))
            input)))
