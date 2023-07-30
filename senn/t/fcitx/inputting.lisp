(defpackage :senn.t.fcitx.inputting
  (:use :cl))
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

(defun resp= (expected consumed-p view)
  (destructuring-bind (consumed-p-expected view-expected)
      expected
    (and (eq consumed-p-expected
             consumed-p)
         (string= view-expected
                  view))))

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

(defmacro buffer-cursor-goes-around-in-the-buffer (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime)))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             t (editing-view :cursor-pos 3
                             :input "あ"
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             t (editing-view :cursor-pos 6
                             :input "ああ"
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
             t (editing-view :cursor-pos 3
                             :input "ああ"
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
             t (editing-view :cursor-pos 0
                             :input "ああ"
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
             t (editing-view :cursor-pos 3
                             :input "ああ"
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
             t (editing-view :cursor-pos 6
                             :input "ああ"
                             :committed-input "")))))

(defmacro buffer-cursor-does-not-go-beyond-the-left-end (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime)))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             t (editing-view :cursor-pos 3
                             :input "あ"
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
             t (editing-view :cursor-pos 0
                             :input "あ"
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65361 :state 0))
             t (editing-view :cursor-pos 0
                             :input "あ"
                             :committed-input "")))))

(defmacro buffer-cursor-does-not-go-beyond-the-right-end (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime)))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             t (editing-view :cursor-pos 3
                             :input "あ"
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65363 :state 0))
             t (editing-view :cursor-pos 3
                             :input "あ"
                             :committed-input "")))))

(defmacro f7-then-katakana (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime)))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 97 :state 0))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65476 :state 0))
             t (editing-view :cursor-pos 3
                             :input "ア"
                             :committed-input "")))))

(defmacro f7-then-nothing-if-empty (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime)))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65476 :state 0))
             nil nil))))

(defmacro katakana-and-enter-then-commit (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime)))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 97 :state 0))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 65476 :state 0))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65293 :state 0))
             t (editing-view :cursor-pos 0
                             :input ""
                             :committed-input "ア")))))

(defmacro enter-then-commit (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime)))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             t (editing-view :cursor-pos 3
                             :input "あ"
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65293 :state 0))
             t (editing-view :cursor-pos 0
                             :input ""
                             :committed-input "あ")))))

(defmacro enter-then-nothing-if-empty (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime)))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65293 :state 0))
             nil nil))))

(defmacro backspace-then-delete (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime)))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 97 :state 0))
             t (editing-view :cursor-pos 3
                             :input "あ"
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65288 :state 0))
             t (editing-view :cursor-pos 0
                             :input ""
                             :committed-input "")))))

(defmacro backspace-then-nothing-if-empty (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime)))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65288 :state 0))
             nil nil))))

(defmacro space-then-full-width-space-if-empty (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime)))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 32 :state 0))
             t (editing-view :cursor-pos 0
                             :input ""
                             :committed-input "　")))))

(defmacro convert-and-char-then-commit (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime :kkc 'kkc)))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 97 :state 0))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 32 :state 0))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 105 :state 0))
             t (editing-view :cursor-pos 3
                             :input "い"
                             :committed-input "亜")))))

(defmacro convert-and-backspace-then-inputting (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime :kkc 'kkc)))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 97 :state 0))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 32 :state 0))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65288 :state 0))
             t (editing-view :cursor-pos 3
                             :input "あ"
                             :committed-input "")))))

(defmacro char-then-predictions (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime :predictor 'predictor)))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 97 :state 0))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 105 :state 0))
             t (editing-view :cursor-pos 6
                             :input "あい"
                             :predictions '("アイ" "いあ")
                             :prediction-index -1
                             :committed-input "")))))

(defmacro tab-then-prediction-selection (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime :predictor 'predictor)))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 97 :state 0))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65289 :state 0))
             t (editing-view :cursor-pos 3
                             :input "ア"
                             :predictions '("ア" "あ")
                             :prediction-index 0
                             :committed-input "")))))

(defmacro tab-then-nothing-if-empty (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime :predictor 'predictor)))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65289 :state 0))
             nil nil))))

(defmacro backspace-then-predictions (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime :predictor 'predictor)))
     (dolist (char '(#\a #\i))
       (senn.fcitx.im.mutable:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65288 :state 0))
             t (editing-view :cursor-pos 3
                             :input "あ"
                             :predictions '("ア" "あ")
                             :prediction-index -1
                             :committed-input "")))))

(defmacro prediction-and-enter-then-commit (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime :predictor 'predictor)))
     (dolist (char '(#\a #\i))
       (senn.fcitx.im.mutable:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 65289 :state 0))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65293 :state 0))
             t (editing-view :cursor-pos 0
                             :input ""
                             :committed-input "アイ")))))

(defmacro prediction-cursor-goes-around (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime :predictor 'predictor)))
     (dolist (char '(#\a #\i))
       (senn.fcitx.im.mutable:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 65289 :state 0))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
             t (editing-view :cursor-pos 6
                             :input "いあ"
                             :predictions '("アイ" "いあ")
                             :prediction-index 1
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
             t (editing-view :cursor-pos 6
                             :input "アイ"
                             :predictions '("アイ" "いあ")
                             :prediction-index 0
                             :committed-input "")))))

(defmacro prediction-cursor-does-not-go-beyond-the-both-ends (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime :predictor 'predictor)))
     (dolist (char '(#\a #\i))
       (senn.fcitx.im.mutable:process-input
        ime (senn.fcitx.keys:make-key :sym (char-code char) :state 0)))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym 65289 :state 0))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
             t (editing-view :cursor-pos 6
                             :input "いあ"
                             :predictions '("アイ" "いあ")
                             :prediction-index 1
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65364 :state 0))
             t (editing-view :cursor-pos 6
                             :input "いあ"
                             :predictions '("アイ" "いあ")
                             :prediction-index 1
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
             t (editing-view :cursor-pos 6
                             :input "アイ"
                             :predictions '("アイ" "いあ")
                             :prediction-index 0
                             :committed-input "")))
     (,test (resp=
             (senn.fcitx.im.mutable:process-input
              ime (senn.fcitx.keys:make-key :sym 65362 :state 0))
             t (editing-view :cursor-pos 6
                             :input "アイ"
                             :predictions '("アイ" "いあ")
                             :prediction-index 0
                             :committed-input "")))))

(defmacro select-candidate-from-prediction (&key test)
  `(let ((ime (senn.fcitx.im.mutable:make-ime :predictor 'predictor)))
     (senn.fcitx.im.mutable:process-input
      ime (senn.fcitx.keys:make-key :sym (char-code #\a) :state 0))
     (,test (resp=
             (senn.fcitx.im.mutable:select-candidate ime 0)
             t (editing-view :cursor-pos 3
                             :input "ア"
                             :predictions '("ア" "あ")
                             :prediction-index 0
                             :committed-input "")))))

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
