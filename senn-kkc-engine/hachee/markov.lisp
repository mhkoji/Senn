(defpackage :senn-kkc-engine.hachee.markov
  (:use :cl)
  (:export :main))
(in-package :senn-kkc-engine.hachee.markov)

(defun read-kkc ()
  ;; We suppose that all the dependency files are put in ~/senn-data.
  (hachee.kkc.impl.markov.read:read-kkc
   :path-word-int-str "~/senn-data/word/int-str.txt"
   :path-word-1gram   "~/senn-data/word/cost-1gram.tsv"
   :path-word-2gram   "~/senn-data/word/cost-2gram.tsv"
   :path-char-int-str "~/senn-data/char/int-str.txt"
   :path-char-1gram   "~/senn-data/char/cost-1gram.tsv"
   :path-char-2gram   "~/senn-data/char/cost-2gram.tsv"
   :path-in-dict      "~/senn-data/in-dict.tsv"))

(defvar *kkc*
  (read-kkc))

(defun main ()
  (senn-kkc-engine.hachee:run *kkc*
                              *standard-input*
                              *standard-output*))
