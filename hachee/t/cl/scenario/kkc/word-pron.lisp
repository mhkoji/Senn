(defpackage :hachee.t.scenario.kkc.word-pron
  (:use :cl)
  (:export :build-and-convert-pronunciations))
(in-package :hachee.t.scenario.kkc.word-pron)

(defun pathnames (system-pathname)
  (list (merge-pathnames "src/kkc/data/aozora/word-pron-utf8/kokoro.txt"
                         system-pathname)))

(defmacro build-and-convert-pronunciations (pathnames &key test)
  `(let ((kkc (hachee.kkc:build-kkc-simple ,pathnames)))
     (,test
      (equal (mapcar #'hachee.kkc.dictionary:unit->key
                     (mapcar #'hachee.kkc.convert:entry-unit
                             (hachee.kkc:convert kkc "わたくしのせんせい")))
             (list "私/わたくし" "の/の" "先生/せんせい")))
     (,test
      (equal (mapcar #'hachee.kkc.dictionary:unit->key
                     (mapcar #'hachee.kkc.convert:entry-unit
                             (hachee.kkc:convert
                              kkc "おとといとうきょうまでうかがいました")))
             (list "おととい/おととい" "東京/とうきょう" "まで/まで"
                   "伺/うかが" "い/い" "ま/ま" "し/し" "た/た")))))
