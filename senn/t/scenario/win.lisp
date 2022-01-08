(defpackage :senn.t.scenario.win
  (:use :cl))
(in-package :senn.t.scenario.win)

(defclass ime (senn.win.stateful-ime:ime)
  ())

(defmethod senn.im.ime:convert ((ime ime) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (assert (string= pron "そら"))
  (list (senn.im.segment:make-segment
         :pron "そら"
         :candidates (list (senn.im.segment:make-candidate
                            :form "空"
                            :origin nil))
         :current-index 0
         :has-more-candidates-p t)))

(defun editing-view (input)
  (let ((json (jsown:new-js
                ("input" input)
                ("predictions" nil))))
    (format nil "EDITING ~A" (jsown:to-json json))))

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
  `(let ((ime (make-instance 'ime
               :state (senn.win.stateful-ime:make-initial-state))))
     (senn.win.stateful-ime:toggle-input-mode ime)
     (,test (string= (senn.win.stateful-ime:process-input
                      ime (senn.win.keys:make-key
                           :code (char-code #\S)))
                     (resp t (editing-view "s"))))
     (,test (string= (senn.win.stateful-ime:process-input
                      ime (senn.win.keys:make-key
                           :code (char-code #\O)))
                     (resp t (editing-view "そ"))))
     (,test (string= (senn.win.stateful-ime:process-input
                      ime (senn.win.keys:make-key
                           :code (char-code #\R)))
                     (resp t (editing-view "そr"))))
     (,test (string= (senn.win.stateful-ime:process-input
                      ime (senn.win.keys:make-key
                           :code (char-code #\A)))
                     (resp t (editing-view "そら"))))
     (,test (string= (senn.win.stateful-ime:process-input
                      ime (senn.win.keys:make-key
                           :code (char-code #\Space)))
                     (resp t (converting-view
                              :forms (list "空")
                              :cursor-form-index 0
                              :cursor-form (list nil 0)))))))

(fiveam:def-suite :senn.win)
(fiveam:in-suite* :senn.win.stateful-ime :in :senn.win)

(fiveam:test test-convert
  (test-convert :test fiveam:is))
