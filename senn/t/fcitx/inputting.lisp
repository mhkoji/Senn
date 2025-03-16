(defpackage :senn.t.fcitx.inputting
  (:use :cl)
  (:import-from :senn.t.fcitx
                :char-key
                :space-key))
(in-package :senn.t.fcitx.inputting)

(defmethod senn.im.kkc:convert ((kkc (eql 'kkc)) (pron string)
                             &key 1st-boundary-index)
  (declare (ignore 1st-boundary-index))
  (assert (string= pron "あ"))
  (list (senn.im.kkc:make-segment
         :pron "あ"
         :candidates (list (senn.im.kkc:make-candidate :form "亜")))))

(defmethod senn.im.predict:execute append ((predictor (eql 'predictor))
                                           (pron string))
  (list (senn.ja:hiragana->katakana pron)
        (reverse pron)))

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

(defmacro buffer-cursor-goes-around-in-the-buffer (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
            t (editing-view :cursor-pos 3
                            :input "あ"
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
            t (editing-view :cursor-pos 6
                            :input "ああ"
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
            t (editing-view :cursor-pos 3
                            :input "ああ"
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
            t (editing-view :cursor-pos 0
                            :input "ああ"
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
            t (editing-view :cursor-pos 3
                            :input "ああ"
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
            t (editing-view :cursor-pos 6
                            :input "ああ"
                            :committed-input ""))))

(defmacro buffer-cursor-does-not-go-beyond-the-left-end (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
            t (editing-view :cursor-pos 3
                            :input "あ"
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
            t (editing-view :cursor-pos 0
                            :input "あ"
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
            t (editing-view :cursor-pos 0
                            :input "あ"
                            :committed-input ""))))

(defmacro buffer-cursor-does-not-go-beyond-the-right-end (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
            t (editing-view :cursor-pos 3
                            :input "あ"
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
            t (editing-view :cursor-pos 3
                            :input "あ"
                            :committed-input ""))))

(defmacro f7-then-katakana (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service)))
     (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65476 :state 0))
            t (editing-view :cursor-pos 3
                            :input "ア"
                            :committed-input ""))))

(defmacro f7-then-nothing-if-empty (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65476 :state 0))
            nil nil)))

(defmacro katakana-and-enter-then-commit (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service)))
     (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym 65476 :state 0))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65293 :state 0))
            t (editing-view :cursor-pos 0
                            :input ""
                            :committed-input "ア"))))

(defmacro enter-then-commit (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
            t (editing-view :cursor-pos 3
                            :input "あ"
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65293 :state 0))
            t (editing-view :cursor-pos 0
                            :input ""
                            :committed-input "あ"))))

(defmacro enter-then-nothing-if-empty (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65293 :state 0))
            nil nil)))

(defmacro backspace-then-delete (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
            t (editing-view :cursor-pos 3
                            :input "あ"
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65288 :state 0))
            t (editing-view :cursor-pos 0
                            :input ""
                            :committed-input ""))))

(defmacro backspace-then-nothing-if-empty (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65288 :state 0))
            nil nil)))

(defmacro space-then-full-width-space-if-empty (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input ime (space-key))
            t (editing-view :cursor-pos 0
                            :input ""
                            :committed-input "　"))))

(defmacro convert-and-char-then-commit (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :kkc 'kkc)))
     (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
     (senn.fcitx.stateful-ime:process-input ime (space-key))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 105 :state 0))
            t (editing-view :cursor-pos 3
                            :input "い"
                            :committed-input "亜"))))

(defmacro convert-and-backspace-then-inputting (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :kkc 'kkc)))
     (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
     (senn.fcitx.stateful-ime:process-input ime (space-key))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65288 :state 0))
            t (editing-view :cursor-pos 3
                            :input "あ"
                            :committed-input ""))))

(defmacro char-then-predictions (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :predictor 'predictor)))
     (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 105 :state 0))
            t (editing-view :cursor-pos 6
                            :input "あい"
                            :predictions '("アイ" "いあ")
                            :prediction-index -1
                            :committed-input ""))))

(defmacro tab-then-prediction-selection (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :predictor 'predictor)))
     (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65289 :state 0))
            t (editing-view :cursor-pos 3
                            :input "ア"
                            :predictions '("ア" "あ")
                            :prediction-index 0
                            :committed-input ""))))

(defmacro tab-then-nothing-if-empty (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :predictor 'predictor)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65289 :state 0))
            nil nil)))

(defmacro backspace-then-predictions (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :predictor 'predictor)))
     (dolist (char '(#\a #\i))
       (senn.fcitx.stateful-ime:process-input ime (char-key char)))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65288 :state 0))
            t (editing-view :cursor-pos 3
                            :input "あ"
                            :predictions '("ア" "あ")
                            :prediction-index -1
                            :committed-input ""))))

(defmacro prediction-and-enter-then-commit (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :predictor 'predictor)))
     (dolist (char '(#\a #\i))
       (senn.fcitx.stateful-ime:process-input ime (char-key char)))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym 65289 :state 0))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65293 :state 0))
            t (editing-view :cursor-pos 0
                            :input ""
                            :committed-input "アイ"))))

(defmacro prediction-cursor-goes-around (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :predictor 'predictor)))
     (dolist (char '(#\a #\i))
       (senn.fcitx.stateful-ime:process-input ime (char-key char)))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym 65289 :state 0))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
            t (editing-view :cursor-pos 6
                            :input "いあ"
                            :predictions '("アイ" "いあ")
                            :prediction-index 1
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
            t (editing-view :cursor-pos 6
                            :input "アイ"
                            :predictions '("アイ" "いあ")
                            :prediction-index 0
                            :committed-input ""))))

(defmacro prediction-cursor-does-not-go-beyond-the-both-ends (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :predictor 'predictor)))
     (dolist (char '(#\a #\i))
       (senn.fcitx.stateful-ime:process-input ime (char-key char)))
     (senn.fcitx.stateful-ime:process-input
      ime (senn.fcitx.keys:make-key :sym 65289 :state 0))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
            t (editing-view :cursor-pos 6
                            :input "いあ"
                            :predictions '("アイ" "いあ")
                            :prediction-index 1
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
            t (editing-view :cursor-pos 6
                            :input "いあ"
                            :predictions '("アイ" "いあ")
                            :prediction-index 1
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
            t (editing-view :cursor-pos 6
                            :input "アイ"
                            :predictions '("アイ" "いあ")
                            :prediction-index 0
                            :committed-input ""))
     (resp= ,test
            (senn.fcitx.stateful-ime:process-input
             ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
            t (editing-view :cursor-pos 6
                            :input "アイ"
                            :predictions '("アイ" "いあ")
                            :prediction-index 0
                            :committed-input ""))))

(defmacro select-candidate-from-prediction (&key test)
  `(let ((ime (senn.fcitx.stateful-ime:make-service :predictor 'predictor)))
     (senn.fcitx.stateful-ime:process-input ime (char-key #\a))
     (resp= ,test
            (senn.fcitx.stateful-ime:select-candidate ime 0)
            t (editing-view :cursor-pos 3
                            :input "ア"
                            :predictions '("ア" "あ")
                            :prediction-index 0
                            :committed-input ""))))

(senn.t.fcitx:add-tests
 :inputting
 buffer-cursor-goes-around-in-the-buffer
 buffer-cursor-does-not-go-beyond-the-left-end
 buffer-cursor-does-not-go-beyond-the-right-end
 f7-then-katakana
 f7-then-nothing-if-empty
 katakana-and-enter-then-commit
 enter-then-commit
 enter-then-nothing-if-empty
 backspace-then-delete
 backspace-then-nothing-if-empty
 space-then-full-width-space-if-empty
 convert-and-backspace-then-inputting
 convert-and-char-then-commit
 char-then-predictions
 tab-then-prediction-selection
 tab-then-nothing-if-empty
 backspace-then-predictions
 prediction-and-enter-then-commit
 prediction-cursor-goes-around
 prediction-cursor-does-not-go-beyond-the-both-ends
 select-candidate-from-prediction)
