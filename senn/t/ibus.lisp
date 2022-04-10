(defpackage :senn.t.ibus
  (:use :cl)
  (:export :run))
(in-package :senn.t.ibus)

(defun resp= (expected consumed-p view)
  (destructuring-bind (consumed-p-expected view-expected)
      expected
    (and (eq consumed-p-expected
             consumed-p)
         (string= view-expected
                  view))))

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

(defun editing-view (&key cursor-pos
                          input
                          predictions
                          prediction-index
                          committed-input)
  (let ((json (jsown:new-js
                ("cursor-pos"       cursor-pos)
                ("input"            input)
                ("predictions"      predictions)
                ("prediction-index" (or prediction-index -1))
                ("committed-input"  committed-input))))
    (format nil "EDITING ~A" (jsown:to-json json))))

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

(defmacro insert-char-when-direct (&key test)
  `(let ((ime (make-ime)))
     (,test (resp=
             (senn.ibus.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym (char-code #\a) :state 0))
             nil nil))))

(defmacro insert-char-when-hiragana (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.ibus.stateful-ime:toggle-input-mode ime)
             "HIRAGANA"))
     (,test (resp=
             (senn.ibus.stateful-ime:process-input
              ime (senn.fcitx.keys:make-key :sym (char-code #\a) :state 0))
             t (editing-view :cursor-pos 3
                             :input "あ"
                             :committed-input "")))))

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

(defun run ()
  (fiveam:run! :senn.t.ibus))

(fiveam:def-suite :senn.t.ibus)
(fiveam:in-suite* :senn.t.ibus)

(fiveam:test test-insert-char-when-direct
  (insert-char-when-direct :test fiveam:is))

(fiveam:test test-insert-char-when-hiragana
  (insert-char-when-hiragana :test fiveam:is))

(fiveam:test test-space-then-covert
  (space-then-convert :test fiveam:is))
