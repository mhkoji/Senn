(defpackage :hachee.t.scenario.kkc.word-pron
  (:use :cl)
  (:export :build-and-convert-pronunciations))
(in-package :hachee.t.scenario.kkc.word-pron)

(defun pathnames (system-pathname)
  (list (merge-pathnames "src/kkc/data/aozora/word-pron-utf8/kokoro.txt"
                         system-pathname)))

(defmacro build-and-convert-pronunciations (system-pathname &key test)
  `(let* ((pathnames (pathnames ,system-pathname))
          (dictionary (hachee.kkc:build-dictionary pathnames))
          (vocabulary (hachee.kkc:build-vocabulary pathnames))
          (language-model (hachee.kkc:build-language-model
                           pathnames
                           :vocabulary vocabulary))
          (kkc (hachee.kkc:make-kkc
                :cost-fn (hachee.kkc.convert.cost-fns:of-word-pron
                          :vocabulary vocabulary
                          :language-model language-model)
                :dictionary dictionary)))
     (,test
      (equal (hachee.kkc:convert kkc "わたくしのせんせい")
             "私/わたくし の/の 先生/せんせい"))
     (,test
      (equal (hachee.kkc:convert kkc "おとといとうきょうまでうかがいました")
             "おととい/おととい 東京/とうきょう まで/まで 伺/うかが い/い ま/ま し/し た/た"))))
