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
          (editing-buffer s)
          (editing-cursor-pos-in-utf-8 s)))


(defmethod make-response ((s converting))
  (let ((input (format nil "~{~A~}"
                       (mapcar #'segment-current-form
                               (converting-segments s)))))
    (format nil "~A ~A ~A~%"
            :converting
            input
            (length (sb-ext:string-to-octets input
                     :external-format :utf-8)))))

(defmethod make-response ((s committed))
  (let ((input (committed-input s)))
    (format nil "~A ~A ~A~%"
            :committed
            input
            (length (sb-ext:string-to-octets input
                                             :external-format :utf-8)))))
