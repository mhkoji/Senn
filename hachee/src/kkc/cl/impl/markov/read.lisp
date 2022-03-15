;; in-dict, int-str, markovは読み込み処理が似ているのでこのパッケージにまとめた。
(defpackage :hachee.kkc.impl.markov.read
  (:use :cl)
  (:export :read-kkc))
(in-package :hachee.kkc.impl.markov.read)

(defun read-int-str (path)
  (let ((str->int (make-hash-table :test #'equal)))
    (with-open-file (stream path)
      (loop for int from 0
            for str = (read-line stream nil nil)
            while str do
        (progn (setf (gethash str str->int) int))))
    (assert (= (gethash "UT" str->int)
               hachee.kkc.impl.markov.int-str:+UT+))
    (assert (= (gethash "BT" str->int)
               hachee.kkc.impl.markov.int-str:+BT+))
    (hachee.kkc.impl.markov.int-str:make-int-str :str->int str->int)))

;;;

(defun read-in-dict (path int-str)
  (let ((hash (make-hash-table :test #'equal)))
    (with-open-file (stream path)
      (loop for line = (read-line stream nil nil) while line do
        (progn
          (destructuring-bind (pron form cost) (cl-ppcre:split "\\t" line)
            (let ((entry (hachee.kkc.impl.markov.in-dict:make-entry
                          :form form
                          :cost (parse-integer cost)
                          :token (hachee.kkc.impl.markov.int-str:to-int
                                  int-str form))))
              (push entry (gethash pron hash)))))))
    (hachee.kkc.impl.markov.in-dict:make-in-dict :hash hash)))

;;;

(defun read-cost-1gram (path int-str)
  (let ((array (make-array (hachee.kkc.impl.markov.int-str:int-str-size
                            int-str))))
    (with-open-file (stream path)
      (loop for line = (read-line stream nil nil) while line do
        (progn
          (destructuring-bind (str cost-str) (cl-ppcre:split "\\t" line)
            (let ((int (hachee.kkc.impl.markov.int-str:to-int int-str str))
                  (cost (parse-integer cost-str)))
              (setf (aref array int) cost))))))
    array))

(defun read-cost-2gram (path int-str)
  (let ((hash (make-hash-table :test #'equal)))
    (with-open-file (stream path)
      (loop for line = (read-line stream nil nil) while line do
        (progn
          (destructuring-bind (i-str j-str cost-str)
              (cl-ppcre:split "\\t" line)
            (let ((i-int (hachee.kkc.impl.markov.int-str:to-int
                          int-str i-str))
                  (j-int (hachee.kkc.impl.markov.int-str:to-int
                          int-str j-str))
                  (cost (parse-integer cost-str)))
              (setf (gethash (list i-int j-int) hash) cost))))))
    hash))

(defun read-markov (1gram-path 2gram-path int-str)
  (hachee.kkc.impl.markov:make-markov
   :cost-1gram (read-cost-1gram 1gram-path int-str)
   :cost-2gram (read-cost-2gram 2gram-path int-str)))

;;;

(defun read-kkc (&key path-word-int-str
                      path-word-1gram
                      path-word-2gram
                      path-char-int-str
                      path-char-1gram
                      path-char-2gram
                      path-in-dict)
  (let* ((word-int-str (read-int-str path-word-int-str))
         (word-markov  (read-markov path-word-1gram
                                    path-word-2gram
                                    word-int-str))
         (char-int-str (read-int-str path-char-int-str))
         (char-markov  (read-markov path-char-1gram
                                    path-char-2gram
                                    char-int-str))
         (in-dict      (read-in-dict path-in-dict word-int-str)))
    (hachee.kkc.impl.markov:make-kkc
     :word-markov  word-markov
     :char-markov  char-markov
     :char-int-str char-int-str
     :in-dict      in-dict)))
