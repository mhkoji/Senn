(defpackage :senn.t.scenario.fcitx.converting
  (:use :cl))
(in-package :senn.t.scenario.fcitx.converting)

(defun resp (consumed-p view)
  (format nil "~A ~A" (if consumed-p 1 0) view))

(defmethod senn.im.kkc:convert ((kkc (eql 'static-kkc)) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (assert (string= pron "きょうは"))
  (list (senn.im.kkc:make-segment :pron "きょう" :form "きょう")
        (senn.im.kkc:make-segment :pron "は" :form "は")))
         
(defmethod senn.im.kkc:list-candidates ((kkc (eql 'static-kkc))
                                        (index number))
  (assert (= index 0))
  (list (senn.im.kkc:make-candidate :form "きょう")
        (senn.im.kkc:make-candidate :form "今日")
        (senn.im.kkc:make-candidate :form "強")))

(defclass ime (senn.fcitx.stateful-ime:ime)
  ())

(defmethod senn.fcitx.im:ime-kkc ((ime ime))
  'static-kkc)


(defmethod senn.im.kkc:convert ((kkc (eql 'nn-kkc)) (pron string)
                                &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (assert (string= pron "ん"))
  (list (senn.im.kkc:make-segment :pron pron :form pron)))
         

(defclass nn-ime (senn.fcitx.stateful-ime:ime)
  ())

(defmethod senn.fcitx.im:ime-kkc ((ime nn-ime))
  'nn-kkc)


(defun make-ime (&optional (class 'ime))
  (let ((state (senn.fcitx.stateful-ime:make-initial-state)))
    (make-instance class :state state)))


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

(defmacro space-then-convert (&key test)
  `(let ((ime (make-ime)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 32 :state 0))
             (resp t (converting-view
                      :forms '("きょう" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates nil
                      :cursor-form-candidate-index 0))))))

(defmacro segment-cursor-goes-around (&key test)
  `(let ((ime (make-ime)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym 32 :state 0))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
             (resp t (converting-view
                      :forms '("きょう" "は")
                      :cursor-form-index 1
                      :cursor-form-candidates nil
                      :cursor-form-candidate-index 0))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
             (resp t (converting-view
                      :forms '("きょう" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates nil
                      :cursor-form-candidate-index 0))))))

(defmacro segment-cursor-does-not-go-beyond-the-both-ends (&key test)
  `(let ((ime (make-ime)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym 32 :state 0))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
             (resp t (converting-view
                      :forms '("きょう" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates nil
                      :cursor-form-candidate-index 0))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
             (resp t (converting-view
                      :forms '("きょう" "は")
                      :cursor-form-index 1
                      :cursor-form-candidates nil
                      :cursor-form-candidate-index 0))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
             (resp t (converting-view
                      :forms '("きょう" "は")
                      :cursor-form-index 1
                      :cursor-form-candidates nil
                      :cursor-form-candidate-index 0))))))

(defmacro space-multiple-times-then-more-candidates (&key test)
  `(let ((ime (make-ime)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym 32 :state 0))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 32 :state 0))
             (resp t (converting-view
                      :forms '("今日" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates (list "きょう" "今日" "強")
                      :cursor-form-candidate-index 1))))))

(defmacro candidate-cursor-goes-around (&key test)
  `(let ((ime (make-ime)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym 32 :state 0))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym 32 :state 0))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
             (resp t (converting-view
                      :forms '("強" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates (list "きょう" "今日" "強")
                      :cursor-form-candidate-index 2))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
             (resp t (converting-view
                      :forms '("今日" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates (list "きょう" "今日" "強")
                      :cursor-form-candidate-index 1))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
             (resp t (converting-view
                      :forms '("きょう" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates (list "きょう" "今日" "強")
                      :cursor-form-candidate-index 0))))))

(defmacro candidate-cursor-loops (&key test)
  `(let ((ime (make-ime)))
     (dolist (char '(#\k #\y #\o #\u #\h #\a))
       (senn.fcitx.stateful-ime:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym 32 :state 0))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym 32 :state 0))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
             (resp t (converting-view
                      :forms '("きょう" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates (list "きょう" "今日" "強")
                      :cursor-form-candidate-index 0))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
             (resp t (converting-view
                      :forms '("強" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates (list "きょう" "今日" "強")
                      :cursor-form-candidate-index 2))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
             (resp t (converting-view
                      :forms '("きょう" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates (list "きょう" "今日" "強")
                      :cursor-form-candidate-index 0))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
             (resp t (converting-view
                      :forms '("今日" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates (list "きょう" "今日" "強")
                      :cursor-form-candidate-index 1))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
             (resp t (converting-view
                      :forms '("強" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates (list "きょう" "今日" "強")
                      :cursor-form-candidate-index 2))))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
             (resp t (converting-view
                      :forms '("きょう" "は")
                      :cursor-form-index 0
                      :cursor-form-candidates (list "きょう" "今日" "強")
                      :cursor-form-candidate-index 0))))))

(defmacro convert-adds-latin-n-to-make-hiragana-letter-n (&key test)
  `(let ((ime (make-ime 'nn-ime)))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym (char-code #\n) :state 0))
     (,test (string=
             (senn.fcitx.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym 32 :state 0))
             (resp t (converting-view
                      :forms '("ん")
                      :cursor-form-index 0
                      :cursor-form-candidates nil
                      :cursor-form-candidate-index 0))))))

(senn.t.scenario.fcitx:add-tests
 :converting
 space-then-convert
 convert-adds-latin-n-to-make-hiragana-letter-n
 segment-cursor-goes-around
 segment-cursor-does-not-go-beyond-the-both-ends
 space-multiple-times-then-more-candidates
 candidate-cursor-goes-around
 candidate-cursor-loops)
