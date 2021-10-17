(defpackage :senn.win.input-processor.view
  (:use :cl
        :senn.win.input-processor.states)
  (:export :editing
           :converting
           :committed
           :can-process))
(in-package :senn.win.input-processor.view)

;; May be viewed as the side-effect to the system by process-input
(defun editing (can-process editing-state)
  (let ((string (senn.buffer:buffer-string
                 (editing-buffer editing-state))))
    (format nil "~A EDITING ~A~%" (if can-process 1 0) string)))

(defun converting (can-process converting-state)
  (let ((json-string
         (jsown:to-json
          (jsown:new-js
           ("forms"
            (mapcar #'senn.segment:segment-current-form
                    (converting-segments converting-state)))
           ("cursor-form-index"
            (converting-current-segment-index converting-state))
           ("cursor-form"
            (let ((segment (converting-current-segment converting-state)))
              (jsown:new-js
               ("candidates"
                (if (senn.segment:segment-has-more-candidates-p segment)
                    nil
                    (senn.segment:segment-forms segment)))
               ("candidate-index"
                (senn.segment:segment-current-index segment)))))))))
    (format nil "~A CONVERTING ~A~%" (if can-process 1 0) json-string)))

(defun committed (string)
  (let ((json-string
         (jsown:to-json
          (jsown:new-js
            ("input" string)))))
    (format nil "1 COMMITTED ~A~%" json-string)))
