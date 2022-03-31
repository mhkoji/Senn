(defpackage :senn.t.scenario.win
  (:use :cl)
  (:export :run))
(in-package :senn.t.scenario.win)

(defmethod senn.im.kkc:convert ((kkc (eql 'static-kkc)) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (assert (string= pron "そら"))
  (list (senn.im.kkc:make-segment
         :pron "そら"
         :candidates (list (senn.im.kkc:make-candidate :form "空")))))

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

(defun make-ime ()
  (make-instance 'senn.win.stateful-ime:ime
   :state (senn.win.stateful-ime:make-initial-state)
   :kkc 'static-kkc
   :predictor nil))

(defmacro test-convert (&key test)
  `(let ((ime (make-ime)))
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

(defun converting-state-segment-strings (state)
  (mapcar (lambda (seg)
            (format nil "~A/~A"
                    (first (senn.im.converting:segment-forms seg))
                    (senn.im.converting:segment-pron seg)))
          (senn.im.converting:state-segments state)))

(defmacro hachee-convert (&key test)
  `(let ((ime (senn.win.stateful-ime:hachee-make-ime
               (senn.im.kkc.hachee:build-hachee-impl-lm-kkc))))
     (,test
      (equal
       (converting-state-segment-strings
        (senn.im.converting:convert ime "とうきょうにいきました"))
      '("東京/とうきょう" "に/に" "行/い" "き/き" "ま/ま" "し/し" "た/た")))))


(defun run ()
  (fiveam:run! :senn.t.scenario.win))

(fiveam:def-suite :senn.t.scenario.win)
(fiveam:in-suite* :senn.t.scenario.win)

(fiveam:test test-convert
  (test-convert :test fiveam:is))

(fiveam:test test-hachee-convert
  (hachee-convert :test fiveam:is))
