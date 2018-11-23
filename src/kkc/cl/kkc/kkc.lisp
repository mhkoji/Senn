(defpackage :hachee.kkc
  (:use :cl)
  (:import-from :alexandria
                :curry)
  (:import-from :hachee.kkc.vocabulary
                :to-int :to-int-or-unk)
  (:export :convert
           :lookup
           :make-kkc
           :build-vocabulary
           :build-dictionary
           :build-language-model))
(in-package :hachee.kkc)

(defun build-dictionary (pathnames)
  (let ((dict (hachee.kkc.dictionary:make-dictionary)))
    (dolist (pathname pathnames dict)
      (dolist (sentence (hachee.kkc.file:file->string-sentences pathname))
        (dolist (word-pron-str (hachee.kkc.file:sentence-units sentence))
          (let ((pron (cadr (cl-ppcre:split "/" word-pron-str))))
            (when pron
              (hachee.kkc.dictionary:add dict
                                         (remove #\- pron)
                                         word-pron-str))))))))

(defun build-vocabulary (pathnames)
  (let ((vocab (hachee.kkc.vocabulary:make-vocabulary)))
    (dolist (pathname pathnames)
      (dolist (sentence (hachee.kkc.file:file->string-sentences pathname))
        (dolist (word-pron-str (hachee.kkc.file:sentence-units sentence))
          (hachee.kkc.vocabulary:add-str vocab word-pron-str))))
    vocab))

(defun build-language-model (pathnames &key vocabulary)
  (let ((model (make-instance 'hachee.language-model.n-gram:model)))
    (let ((sentences nil))
      (dolist (pathname pathnames)
        (dolist (sentence (hachee.kkc.file:file->string-sentences pathname))
          (let ((word-pron-list (hachee.kkc.file:sentence-units sentence)))
            (push (hachee.language-model:make-sentence
                   :tokens (mapcar (curry #'to-int-or-unk vocabulary)
                                   word-pron-list))
                  sentences))))
      (hachee.language-model.n-gram:train model sentences
       :BOS (to-int vocabulary hachee.kkc.vocabulary:+BOS+)
       :EOS (to-int vocabulary hachee.kkc.vocabulary:+EOS+)))
    model))


(defstruct kkc cost-fn dictionary)

(defun convert (kkc pronunciation)
  (hachee.kkc.convert:execute pronunciation
   :cost-fn (kkc-cost-fn kkc)
   :dictionary (kkc-dictionary kkc)))

(defun lookup (kkc pronunciation)
  (hachee.kkc.lookup:execute pronunciation
   :dictionary (kkc-dictionary kkc)))
