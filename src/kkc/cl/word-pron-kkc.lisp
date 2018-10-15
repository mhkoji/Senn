(defpackage :hachee.kkc.word-pron
  (:use :cl :hachee.kkc)
  (:import-from :alexandria
                :curry)
  (:import-from :hachee.kkc.vocabulary
                :to-int :to-int-or-unk)
  (:export :build))
(in-package :hachee.kkc.word-pron)

(defclass word-pron-kkc (kkc)
  ((language-model
    :initform nil
    :initarg :language-model
    :reader word-pron-kkc-language-model)))

(defmethod transition-log-probability ((kkc word-pron-kkc)
                                       (word-pron string)
                                       (history-word-pron-list list))
  (let ((vocab (kkc-vocabulary kkc)))
    (let ((p (hachee.language-model:transition-probability
              (word-pron-kkc-language-model kkc)
              (to-int-or-unk vocab word-pron)
              (mapcar (curry #'to-int-or-unk vocab)
                      history-word-pron-list))))
      (if (= p 0) -100000 (log p)))))

(defun build-word-pron-language-model (pathnames &key vocabulary)
  (let ((model (make-instance 'hachee.language-model:model
                :n-gram (hachee.language-model.n-gram:make-n-gram))))
    (let ((sentences nil))
      (dolist (pathname pathnames)
        (dolist (sentence (hachee.kkc.file:file->string-sentences pathname))
          (let ((word-pron-list (hachee.kkc.file:sentence-units sentence)))
            (push (hachee.language-model:make-sentence
                   :tokens (mapcar (curry #'to-int-or-unk vocabulary)
                                   word-pron-list))
                  sentences))))
      (hachee.language-model:train model sentences
       :BOS (to-int vocabulary hachee.kkc.vocabulary:+BOS+)
       :EOS (to-int vocabulary hachee.kkc.vocabulary:+EOS+)))
    model))

(defun build (pathnames)
  (let ((vocabulary (build-word-pron-vocabulary pathnames))
        (dictionary (build-word-pron-dictionary pathnames)))
    (let ((language-model (build-word-pron-language-model
                           pathnames :vocabulary vocabulary)))
      (make-instance 'word-pron-kkc
                     :vocabulary vocabulary
                     :dictionary dictionary
                     :language-model language-model))))
