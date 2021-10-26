(defpackage :senn.t.scenario.win
  (:use :cl))
(in-package :senn.t.scenario.win)

(defclass ime (senn.im:ime) ())

(defmethod senn.im:convert ((ime ime) (pron string)
                            &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (assert (string= pron "そら"))
  (list (senn.segment:make-segment
         :pron pron
         :candidates (list (senn.segment:make-candidate
                            :form "空"
                            :origin nil))
         :has-more-candidates-p t
         :current-index 0)))

(defun converting-view (&key forms cursor-form-index cursor-form)
  (let ((json-string
         (jsown:to-json
          (jsown:new-js
           ("forms" forms)
           ("cursor-form-index" cursor-form-index)
           ("cursor-form"
            (jsown:new-js
              ("candidates" (first cursor-form))
              ("candidate-index" (second cursor-form))))))))
    (format nil "CONVERTING ~A" json-string)))

(defun resp (can-process view)
  (format nil "~A ~A~%" (if can-process 1 0) view))

(defmacro test-convert (&key test)
  `(let ((im (senn.win.stateful-ime:make-im
              (make-instance 'ime))))
     (senn.win.stateful-ime:toggle-input-mode im)
     (,test (string= (senn.win.stateful-ime:process-input
                      im (senn.win.keys:make-key
                          :code (char-code #\S)))
                     (resp t "EDITING s")))
     (,test (string= (senn.win.stateful-ime:process-input
                      im (senn.win.keys:make-key
                          :code (char-code #\O)))
                     (resp t "EDITING そ")))
     (,test (string= (senn.win.stateful-ime:process-input
                      im (senn.win.keys:make-key
                          :code (char-code #\R)))
                     (resp t "EDITING そr")))
     (,test (string= (senn.win.stateful-ime:process-input
                      im (senn.win.keys:make-key
                          :code (char-code #\A)))
                     (resp t "EDITING そら")))
     (,test (string= (senn.win.stateful-ime:process-input
                      im (senn.win.keys:make-key
                          :code (char-code #\Space)))
                     (resp t (converting-view
                              :forms (list "空")
                              :cursor-form-index 0
                              :cursor-form (list nil 0)))))))

(fiveam:def-suite :senn.win)
(fiveam:in-suite* :senn.win.stateful-ime :in :senn.win)

(fiveam:test test-convert
  (test-convert :test fiveam:is))
