(defpackage :senn.t.scenario.fcitx.converting
  (:use :cl))
(in-package :senn.t.scenario.fcitx.converting)

(defun resp (consumed-p view)
  (format nil "~A ~A" (if consumed-p 1 0) view))

(defclass kkc ()
  ())

(defmethod senn.im.kkc:convert ((kkc kkc) (pron string)
                                &key 1st-boundary-index)
  (assert (string= pron "きょうは"))
  (list (senn.im.segment:make-segment
         :pron "きょう"
         :candidates (list (senn.im.segment:make-candidate
                            :form "きょう"))
         :current-index 0
         :has-more-candidates-p t)
        (senn.im.segment:make-segment
         :pron "は"
         :candidates (list (senn.im.segment:make-candidate
                            :form "は"))
         :current-index 0
         :has-more-candidates-p t)))

(defmethod senn.im.kkc:lookup ((kkc kkc) (pron string)
                               &key next prev)
  (assert (string= pron "きょう"))
  (list (senn.im.segment:make-candidate :form "きょう")
        (senn.im.segment:make-candidate :form "今日")
        (senn.im.segment:make-candidate :form "強")))

(defclass ime (senn.fcitx.stateful-ime:ime)
  ())

(defmethod senn.im.ime:ime-kkc ((ime ime))
  (make-instance 'kkc))

(defun make-ime ()
  (let ((state (senn.fcitx.stateful-ime:make-initial-state)))
    (make-instance 'ime :state state)))

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


(defmacro add-tests (&rest syms)
  `(progn
     ,@(mapcar (lambda (sym)
                 `(fiveam:test ,sym (,sym :test fiveam:is)))
               syms)))


(fiveam:in-suite* :senn.fcitx.converting :in :senn.fcitx)

(add-tests
 space-then-convert
 segment-cursor-goes-around
 segment-cursor-does-not-go-beyond-the-both-ends
 space-multiple-times-then-more-candidates
 candidate-cursor-goes-around
 candidate-cursor-loops)

