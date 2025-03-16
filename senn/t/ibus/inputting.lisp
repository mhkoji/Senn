(defpackage :senn.t.ibus.inputting
  (:use :cl))
(in-package :senn.t.ibus.inputting)

(defmacro resp= (test expected consumed-p view)
  `(destructuring-bind (consumed-p-expected view-expected)
       ,expected
     (,test (eq consumed-p-expected ,consumed-p))
     (,test (string= view-expected ,view))))

(defun editing-view (&key cursor-pos
                          input
                          predictions
                          prediction-index
                          committed-input)
  (let ((view
         (yason:with-output-to-string* ()
           (yason:encode
            (alexandria:plist-hash-table
             (list
              "cursor-pos"       cursor-pos
              "input"            input
              "predictions"      (or predictions #())
              "prediction-index" (or prediction-index -1)
              "committed-input"  committed-input)
             :test #'equal)))))
    (format nil "EDITING ~A" view)))

(defun make-ime ()
  (senn.ibus.stateful-ime:make-service :kkc nil))

(defmacro insert-char-when-direct (&key test)
  `(let ((ime (make-ime)))
     (resp= ,test
            (senn.ibus.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym (char-code #\a) :state 0))
            nil nil)))

(defmacro insert-char-when-hiragana (&key test)
  `(let ((ime (make-ime)))
     (,test (string=
             (senn.ibus.stateful-ime:toggle-input-mode ime)
             "HIRAGANA"))
     (resp= ,test
            (senn.ibus.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym (char-code #\a) :state 0))
            t (editing-view :cursor-pos 3
                            :input "あ"
                            :committed-input ""))))

(senn.t.ibus:add-tests
  :inputting
  insert-char-when-direct
  insert-char-when-hiragana)
