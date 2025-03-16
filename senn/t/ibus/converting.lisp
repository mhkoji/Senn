(defpackage :senn.t.ibus.converting
  (:use :cl)
  (:export :run))
(in-package :senn.t.ibus.converting)

(defmacro resp= (test expected consumed-p view)
  `(destructuring-bind (consumed-p-expected view-expected)
       ,expected
     (,test (eq consumed-p-expected ,consumed-p))
     (,test (string= view-expected ,view))))

(defmethod senn.im.kkc:convert ((kkc (eql 'static-kkc)) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (assert (string= pron "きょうは"))
  (list (senn.im.kkc:make-segment
         :pron "きょう"
         :candidates (list (senn.im.kkc:make-candidate :form "きょう")))
        (senn.im.kkc:make-segment
         :pron "は"
         :candidates (list (senn.im.kkc:make-candidate :form "は")))))

(defmethod senn.im.kkc:list-candidates ((kkc (eql 'static-kkc))
                                        (pron string))
  (assert (string= pron "きょう"))
  (list (senn.im.kkc:make-candidate :form "きょう")
        (senn.im.kkc:make-candidate :form "今日")
        (senn.im.kkc:make-candidate :form "強")))

(defun converting-view (&key forms
                             cursor-form-index
                             cursor-form-candidates
                             cursor-form-candidate-index)
  (let ((view
         (yason:with-output-to-string* ()
           (yason:encode
            (alexandria:plist-hash-table
             (list
              "forms"             forms
              "cursor-form-index" cursor-form-index
              "cursor-form"
              (alexandria:plist-hash-table
               (list
                "candidates"      (or cursor-form-candidates #())
                "candidate-index" cursor-form-candidate-index)
               :test #'equal))
             :test #'equal)))))
    (format nil "CONVERTING ~A" view)))

(defun make-ime ()
  (make-instance 'senn.ibus.stateful-ime:service
   :ime (make-instance 'senn.ibus.im:ime :kkc 'static-kkc)))

(defmacro space-then-convert (&key test)
  `(let ((ime (make-ime)))
     (senn.ibus.stateful-ime:toggle-input-mode ime)
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.ibus.stateful-ime:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (resp= ,test
            (senn.ibus.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 32 :state 0))
            t (converting-view
               :forms '("きょう" "は")
               :cursor-form-index 0
               :cursor-form-candidates nil
               :cursor-form-candidate-index 0))))

(senn.t.ibus:add-tests
  :converting
  space-then-convert)
