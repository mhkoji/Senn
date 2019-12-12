(defpackage :hachee.kkc.simple.convert
  (:use :cl)
  (:import-from :hachee.kkc.convert
                :node-word
                :node-word-origin)
  (:import-from :hachee.language-model.n-gram
                :transition-probability
                :sentence-log-probability)
  (:import-from :hachee.language-model.vocabulary
                :to-int-or-unk
                :to-int-or-nil)
  (:export :get-score-fn))
(in-package :hachee.kkc.simple.convert)

(defun get-score-fn (&key word-vocabulary word-n-gram-model)
  (let ((fail-safe-score -10000))
    (lambda (curr-node prev-node)
      (let ((curr-token (to-int-or-nil word-vocabulary
                                       (node-word curr-node)))
            (prev-token (to-int-or-unk word-vocabulary
                                       (node-word prev-node))))
        (if curr-token
            (let ((p (transition-probability
                      word-n-gram-model curr-token (list prev-token))))
              (if (/= p 0)
                  (log p)
                  fail-safe-score))
            fail-safe-score)))))
