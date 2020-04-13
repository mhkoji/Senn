(defpackage :hachee.kkc.simple
  (:use :cl)
  (:import-from :hachee.kkc.build
                :build-vocabulary
                :build-dictionary
                :train-n-gram-model)
  (:export :kkc
           :create-kkc))
(in-package :hachee.kkc.simple)

(defstruct (kkc (:include hachee.kkc:kkc)))

;;; Convert
(defmethod hachee.kkc:get-convert-score-fn ((kkc kkc))
  (let ((word-n-gram-model (kkc-n-gram-model kkc)))
    (lambda (curr-entry prev-entry)
      (let ((p (hachee.language-model.n-gram:transition-probability
                word-n-gram-model
                (hachee.kkc.entry:entry-token curr-entry)
                (list (hachee.kkc.entry:entry-token prev-entry)))))
        (if (/= p 0)
            (log p)
            -10000)))))

;;; Create
(defun create-kkc (pathnames &key char-dictionary)
  (let ((vocabulary (build-vocabulary pathnames))
        (n-gram-model (make-instance 'hachee.language-model.n-gram:model)))
    (train-n-gram-model n-gram-model pathnames vocabulary)
    (make-kkc
     :n-gram-model n-gram-model
     :vocabulary vocabulary
     :vocabulary-dictionary dictionary
     :word-dictionary (build-dictionary pathnames vocabulary)
     :char-dictionary (or char-dictionary
                          (hachee.kkc.word.dictionary:make-dictionary))
     :extended-dictionary (hachee.kkc.word.dictionary:make-dictionary))))
