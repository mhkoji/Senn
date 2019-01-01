(defpackage :hachee.input-method.buffer
  (:use :cl)
  (:export :insert-char
           :try-move-cursor-pos)
  (:import-from :alexandria
                :if-let))
(in-package :hachee.input-method.buffer)

(defun insert-char (buffer pos char)
  (labels ((try-insertion (diff-from-pos)
             (if (< diff-from-pos 0)
                 (list (concatenate 'string
                                    (subseq buffer 0 pos)
                                    (string char)
                                    (subseq buffer pos))
                       (1+ pos))
                 (let ((substr-ending-at-pos
                        (concatenate 'string
                                     (subseq buffer
                                             (- pos diff-from-pos) pos)
                                     (string char))))
                   (if-let ((hiragana (hachee.ja:romaji->hiragana
                                       substr-ending-at-pos)))
                     (list (concatenate 'string
                                        (subseq buffer
                                                0 (- pos diff-from-pos))
                                        hiragana
                                        (subseq buffer pos))
                           (+ (- pos diff-from-pos)
                              (length hiragana)))
                     (try-insertion (1- diff-from-pos)))))))
    (try-insertion (min pos 4))))


(defun try-move-cursor-pos (buffer pos diff)
  (let ((new-pos (+ diff pos)))
    (when (<= 0 new-pos (length buffer))
      new-pos)))


(destructuring-bind (buf pos) (insert-char "あk" 2 #\a)
  (assert (string= buf "あか"))
  (assert (= pos 2)))

(destructuring-bind (buf pos) (insert-char "kk" 1 #\a)
  (assert (string= buf "かk"))
  (assert (= pos 1)))
