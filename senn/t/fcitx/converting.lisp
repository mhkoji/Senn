(defpackage :senn.t.fcitx.converting
  (:use :cl)
  (:import-from :senn.t.fcitx
                :char-key
                :space-key))
(in-package :senn.t.fcitx.converting)

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

(defmethod senn.im.kkc:convert ((kkc (eql 'nn-kkc)) (pron string)
                             &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (assert (string= pron "ん"))
  (list (senn.im.kkc:make-segment
         :pron pron
         :candidates (list (senn.im.kkc:make-candidate :form pron)))))


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

(defmacro space-then-convert (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :kkc 'static-kkc)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input ime (char-key char)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input ime (space-key))
            t (converting-view
               :forms '("きょう" "は")
               :cursor-form-index 0
               :cursor-form-candidates nil
               :cursor-form-candidate-index 0))))

(defmacro segment-cursor-goes-around (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :kkc 'static-kkc)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input ime (char-key char)))
     (senn.fcitx.stateful-ime:process-input ime (space-key))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
            t (converting-view
               :forms '("きょう" "は")
               :cursor-form-index 1
               :cursor-form-candidates nil
               :cursor-form-candidate-index 0))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
            t (converting-view
               :forms '("きょう" "は")
               :cursor-form-index 0
               :cursor-form-candidates nil
               :cursor-form-candidate-index 0))))

(defmacro segment-cursor-does-not-go-beyond-the-both-ends (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :kkc 'static-kkc)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input ime (char-key char)))
     (senn.fcitx.stateful-ime:process-input ime (space-key))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
            t (converting-view
               :forms '("きょう" "は")
               :cursor-form-index 0
               :cursor-form-candidates nil
               :cursor-form-candidate-index 0))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
            t (converting-view
               :forms '("きょう" "は")
               :cursor-form-index 1
               :cursor-form-candidates nil
               :cursor-form-candidate-index 0))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
            t (converting-view
               :forms '("きょう" "は")
               :cursor-form-index 1
               :cursor-form-candidates nil
               :cursor-form-candidate-index 0))))

(defmacro space-multiple-times-then-more-candidates (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :kkc 'static-kkc)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input ime (char-key char)))
     (senn.fcitx.stateful-ime:process-input ime (space-key))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input ime (space-key))
            t (converting-view
               :forms '("今日" "は")
               :cursor-form-index 0
               :cursor-form-candidates (list "きょう" "今日" "強")
               :cursor-form-candidate-index 1))))

(defmacro candidate-cursor-goes-around (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :kkc 'static-kkc)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input ime (char-key char)))
     (senn.fcitx.stateful-ime:process-input ime (space-key))
     (senn.fcitx.stateful-ime:process-input ime (space-key))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
            t (converting-view
               :forms '("強" "は")
               :cursor-form-index 0
               :cursor-form-candidates (list "きょう" "今日" "強")
               :cursor-form-candidate-index 2))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
            t (converting-view
               :forms '("今日" "は")
               :cursor-form-index 0
               :cursor-form-candidates (list "きょう" "今日" "強")
               :cursor-form-candidate-index 1))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
            t (converting-view
               :forms '("きょう" "は")
               :cursor-form-index 0
               :cursor-form-candidates (list "きょう" "今日" "強")
               :cursor-form-candidate-index 0))))

(defmacro candidate-cursor-loops (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :kkc 'static-kkc)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input ime (char-key char)))
     (senn.fcitx.stateful-ime:process-input ime (space-key))
     (senn.fcitx.stateful-ime:process-input ime (space-key))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
            t (converting-view
               :forms '("きょう" "は")
               :cursor-form-index 0
               :cursor-form-candidates (list "きょう" "今日" "強")
               :cursor-form-candidate-index 0))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
            t (converting-view
               :forms '("強" "は")
               :cursor-form-index 0
               :cursor-form-candidates (list "きょう" "今日" "強")
               :cursor-form-candidate-index 2))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
            t (converting-view
               :forms '("きょう" "は")
               :cursor-form-index 0
               :cursor-form-candidates (list "きょう" "今日" "強")
               :cursor-form-candidate-index 0))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
            t (converting-view
               :forms '("今日" "は")
               :cursor-form-index 0
               :cursor-form-candidates (list "きょう" "今日" "強")
               :cursor-form-candidate-index 1))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
            t (converting-view
               :forms '("強" "は")
               :cursor-form-index 0
               :cursor-form-candidates (list "きょう" "今日" "強")
               :cursor-form-candidate-index 2))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
            t (converting-view
               :forms '("きょう" "は")
               :cursor-form-index 0
               :cursor-form-candidates (list "きょう" "今日" "強")
               :cursor-form-candidate-index 0))))

(defmacro convert-adds-latin-n-to-make-hiragana-letter-n (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :kkc 'nn-kkc)))
     (senn.fcitx.stateful-ime:process-input ime (char-key #\n))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input ime (space-key))
            t (converting-view
               :forms '("ん")
               :cursor-form-index 0
               :cursor-form-candidates nil
               :cursor-form-candidate-index 0))))

(defmacro select-candidate (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :kkc 'static-kkc)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input ime (char-key char)))
     (senn.fcitx.stateful-ime:process-input ime (space-key))
     (senn.fcitx.stateful-ime:process-input ime (space-key))
     (resp= ,test
            (senn.fcitx.stateful-ime:select-candidate ime 2)
            t (converting-view
               :forms '("強" "は")
               :cursor-form-index 0
               :cursor-form-candidates (list "きょう" "今日" "強")
               :cursor-form-candidate-index 2))))

(senn.t.fcitx:add-tests
 :converting
 space-then-convert
 convert-adds-latin-n-to-make-hiragana-letter-n
 segment-cursor-goes-around
 segment-cursor-does-not-go-beyond-the-both-ends
 space-multiple-times-then-more-candidates
 candidate-cursor-goes-around
 candidate-cursor-loops
 select-candidate)
