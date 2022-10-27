(defpackage :senn.t.ibus.converting
  (:use :cl)
  (:export :run))
(in-package :senn.t.ibus.converting)

(defun resp= (expected consumed-p view)
  (destructuring-bind (consumed-p-expected view-expected)
      expected
    (and (eq consumed-p-expected
             consumed-p)
         (string= view-expected
                  view))))

(defmethod senn-kkc:convert ((kkc (eql 'static-kkc)) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (assert (string= pron "きょうは"))
  (list (senn-kkc:make-segment
         :pron "きょう"
         :candidates (list (senn-kkc:make-candidate :form "きょう")))
        (senn-kkc:make-segment
         :pron "は"
         :candidates (list (senn-kkc:make-candidate :form "は")))))

(defmethod senn-kkc:list-candidates ((kkc (eql 'static-kkc))
                                        (pron string))
  (assert (string= pron "きょう"))
  (list (senn-kkc:make-candidate :form "きょう")
        (senn-kkc:make-candidate :form "今日")
        (senn-kkc:make-candidate :form "強")))

(defun converting-view (&key forms
                             cursor-form-index
                             cursor-form-candidates
                             cursor-form-candidate-index)
  (let ((json
         (jsown:new-js
           ("forms"             forms)
           ("cursor-form-index" cursor-form-index)
           ("cursor-form"
            (jsown:new-js
              ("candidates"      cursor-form-candidates)
              ("candidate-index" cursor-form-candidate-index))))))
    (format nil "CONVERTING ~A" (jsown:to-json json))))

(defun make-ime ()
  (senn.ibus.stateful-ime:make-ime
   :kkc 'static-kkc))

(defmacro space-then-convert (&key test)
  `(let ((ime (make-ime)))
     (senn.ibus.stateful-ime:toggle-input-mode ime)
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.ibus.stateful-ime:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (,test (resp=
             (senn.ibus.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 32 :state 0))
             t (converting-view
                :forms '("きょう" "は")
                :cursor-form-index 0
                :cursor-form-candidates nil
                :cursor-form-candidate-index 0)))))

(senn.t.ibus:add-tests
  :converting
  space-then-convert)
