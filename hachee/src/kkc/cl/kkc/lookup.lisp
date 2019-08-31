(defpackage :hachee.kkc.lookup
  (:use :cl)
  (:export :execute
           :item-word
           :item-origin))
(in-package :hachee.kkc.lookup)

(defstruct item word origin)

(defun execute (pronunciation &key vocabulary-dictionary
                                   tankan-dictionary)
  (let ((result-items nil))
    (loop
      for (dict origin) in (list
                            (list vocabulary-dictionary :vocabulary)
                            (list tankan-dictionary :tankan-dictionary))
      do (progn
           (dolist (word (hachee.kkc.word.dictionary:lookup
                          dict
                          pronunciation))
             (let ((item (make-item :word word :origin origin)))
               (pushnew item result-items
                        :test #'equal
                        :key (lambda (item)
                               (hachee.kkc.word:word->key
                                (item-word item))))))))
    (nreverse result-items)))
