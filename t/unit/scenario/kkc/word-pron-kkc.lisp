(defpackage :hachee.t.scenario.kkc.word-pron-kkc
  (:use :cl)
  (:export :build-and-convert-pronunciations))
(in-package :hachee.t.scenario.kkc.word-pron-kkc)

(defun pathnames (system-pathname)
  (list (merge-pathnames "src/kkc/data/aozora/kokoro.word-pron.utf8.txt"
                         system-pathname)))

(defmacro build-and-convert-pronunciations (system-pathname &key test)
  `(let ((kkc (hachee.kkc.word-pron:build (pathnames ,system-pathname))))
     (,test
      (equal (hachee.kkc:convert kkc "わたくしのせんせい")
             "私/わたくし の/の 先生/せんせい"))
     (,test
      (equal (hachee.kkc:convert kkc "おとといとうきょうまでうかがいました")
             "おととい/おととい 東京/とうきょう まで/まで 伺/うかが い/い ま/ま し/し た/た"))))
