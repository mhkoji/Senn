(defpackage :hachee.t.scenario.kkc.word-pron
  (:use :cl)
  (:export :build-and-convert-pronunciations))
(in-package :hachee.t.scenario.kkc.word-pron)

(defun entry->string (e)
  (format nil "~A/~A"
          (hachee.kkc.convert:entry-form e)
          (hachee.kkc.convert:entry-pron e)))

(defmacro build-and-convert-pronunciations (&key test)
  `(let ((kkc (hachee.kkc.impl.lm:build-kkc-simple
               (hachee.data.corpus:word-pron-utf8-pathnames))))
     (,test
      (equal (mapcar #'entry->string
                     (hachee.kkc.convert:execute
                      kkc "わたくしのせんせい"))
             (list "私/わたくし" "の/の" "先生/せんせい")))
     (,test
      (equal (mapcar #'entry->string
                     (hachee.kkc.convert:execute
                      kkc "おとといとうきょうまでうかがいました"))
             (list "おととい/おととい" "東京/とうきょう" "まで/まで"
                   "伺/うかが" "い/い" "ま/ま" "し/し" "た/た")))))
