(defpackage :senn-kkc-engine.hachee.markov
  (:use :cl)
  (:export :set-kkc
           :main))
(in-package :senn-kkc-engine.hachee.markov)

(defun read-kkc (markov-dir)
  (labels ((make-path (path)
             (format nil "~A~A" markov-dir path)))
    (hachee.kkc.impl.markov.read:read-kkc
     :path-word-int-str (make-path "word/int-str.txt")
     :path-word-1gram   (make-path "word/cost-1gram.tsv")
     :path-word-2gram   (make-path "word/cost-2gram.tsv")
     :path-char-int-str (make-path "char/int-str.txt")
     :path-char-1gram   (make-path "char/cost-1gram.tsv")
     :path-char-2gram   (make-path "char/cost-2gram.tsv")
     :path-in-dict      (make-path "in-dict.tsv"))))

(defvar *kkc*)

(defun set-kkc (&optional (markov-dir "~/senn-data/"))
  (setq *kkc* (read-kkc markov-dir)))

(defun main ()
  (senn-kkc-engine.hachee:run *kkc*
                              *standard-input*
                              *standard-output*))
