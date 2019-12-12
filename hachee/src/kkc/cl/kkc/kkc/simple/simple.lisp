(defpackage :hachee.kkc.simple
  (:use :cl)
  (:import-from :hachee.kkc.build
                :build-vocabulary
                :build-dictionary
                :build-n-gram-model)
  (:export :kkc
           :create-kkc))
(in-package :hachee.kkc.simple)

(defstruct (kkc (:include hachee.kkc:kkc)))

;;; Convert
(defmethod hachee.kkc:get-score-fn ((kkc kkc))
  (hachee.kkc.simple.convert:get-score-fn
   :word-vocabulary (kkc-vocabulary kkc)
   :word-n-gram-model (kkc-n-gram-model kkc)))


;;; Create
(defun create-kkc (pathnames &key word-dictionary tankan-dictionary)
  (let* ((vocabulary (build-vocabulary pathnames))
         (dictionary (build-dictionary pathnames vocabulary))
         (n-gram-model (build-n-gram-model pathnames vocabulary)))
    (make-kkc
     :n-gram-model n-gram-model
     :vocabulary vocabulary
     :vocabulary-dictionary dictionary
     :extended-dictionary (hachee.kkc.word.dictionary:make-dictionary)
     :word-dictionary (or word-dictionary
                          (hachee.kkc.word.dictionary:make-dictionary))
     :tankan-dictionary (or tankan-dictionary
                            (hachee.kkc.word.dictionary:make-dictionary)))))
