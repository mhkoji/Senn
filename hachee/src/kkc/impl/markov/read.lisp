;; in-dict, int-str, markovは読み込み処理が似ているのでこのパッケージにまとめた。
(defpackage :hachee.kkc.impl.markov.read
  (:use :cl)
  (:export :read-kkc-dir
           :read-kkc-paths)
  (:local-nicknames (:int-str :hachee.kkc.impl.markov.int-str)))
(in-package :hachee.kkc.impl.markov.read)

(defun read-int-str (path)
  (let ((str->int (make-hash-table :test #'equal)))
    (with-open-file (stream path :external-format :utf-8)
      (loop for int from 0
            for str = (read-line stream nil nil)
            while str do
        (progn (setf (gethash str str->int) int))))
    (assert (= (gethash "UT" str->int) int-str:+UT+))
    (assert (= (gethash "BT" str->int) int-str:+BT+))
    (int-str:make-int-str :str->int str->int)))

;;;

(defun read-in-dict (path int-str)
  (let ((hash (make-hash-table :test #'equal)))
    (with-open-file (stream path :external-format :utf-8)
      (loop for line = (read-line stream nil nil) while line do
        (progn
          (destructuring-bind (pron form cost) (cl-ppcre:split "\\t" line)
            (let ((entry (hachee.kkc.impl.markov.in-dict:make-entry
                          :form form
                          :cost (parse-integer cost)
                          :token (int-str:to-int int-str form))))
              (push entry (gethash pron hash)))))))
    (hachee.kkc.impl.markov.in-dict:make-in-dict :hash hash)))

;;;

(defun read-cost-1gram (path int-str)
  (let ((array (make-array (int-str:int-str-size int-str))))
    (with-open-file (stream path :external-format :utf-8)
      (loop for line = (read-line stream nil nil) while line do
        (progn
          (destructuring-bind (str cost-str) (cl-ppcre:split "\\t" line)
            (let ((int (int-str:to-int int-str str))
                  (cost (parse-integer cost-str)))
              (setf (aref array int) cost))))))
    array))

(defun read-cost-2gram (path int-str)
  (let ((hash (make-hash-table :test #'equal)))
    (with-open-file (stream path :external-format :utf-8)
      (loop for line = (read-line stream nil nil) while line do
        (progn
          (destructuring-bind (i-str j-str cost-str)
              (cl-ppcre:split "\\t" line)
            (let ((i-int (int-str:to-int int-str i-str))
                  (j-int (int-str:to-int int-str j-str))
                  (cost (parse-integer cost-str)))
              (setf (gethash (list i-int j-int) hash) cost))))))
    hash))

(defun read-markov (1gram-path 2gram-path int-str)
  (hachee.kkc.impl.markov:make-markov
   :cost-1gram (read-cost-1gram 1gram-path int-str)
   :cost-2gram (read-cost-2gram 2gram-path int-str)))

;;;

(defun char-cost-0gram (char-int-str)
  (let ((pron-alphabet-size 6878)
        (char-int-str-size  (int-str:int-str-size char-int-str)))
    ;; 2 for UT and BT
    (let ((unk-char-size (- pron-alphabet-size (- char-int-str-size 2))))
      (assert (< 0 unk-char-size))
      (hachee.kkc.impl.markov:probability->cost (/ 1 unk-char-size)))))

(defun in-dict-prob (in-dict char-int-str char-markov char-cost-0gram)
  (let ((sum-prob 0))
    (hachee.kkc.impl.markov.in-dict:do-entries (entry in-dict)
      (let* ((form (hachee.kkc.impl.markov.in-dict:entry-form entry))
             (cost (hachee.kkc.impl.markov:char-based-cost
                    form char-int-str char-markov char-cost-0gram))
             (prob (hachee.kkc.impl.markov:cost->probability cost)))
        (incf sum-prob prob)))
    sum-prob))

(defvar *empty-ex-dict*
  (hachee.kkc.impl.markov.ex-dict:make-ex-dict
   :hash (make-hash-table)))

(defun read-kkc-paths (&key path-word-int-str
                            path-word-1gram
                            path-word-2gram
                            path-char-int-str
                            path-char-1gram
                            path-char-2gram
                            path-in-dict)
  (let* ((word-int-str (read-int-str path-word-int-str))
         (in-dict      (read-in-dict path-in-dict word-int-str))
         (word-markov  (read-markov path-word-1gram
                                    path-word-2gram
                                    word-int-str))
         (char-int-str (read-int-str path-char-int-str))
         (char-markov  (read-markov path-char-1gram
                                    path-char-2gram
                                    char-int-str))
         (char-cost-0gram (char-cost-0gram char-int-str))
         (in-dict-prob    (in-dict-prob in-dict
                                        char-int-str
                                        char-markov
                                        char-cost-0gram)))
    (hachee.kkc.impl.markov:make-kkc
     :word-markov  word-markov
     :in-dict      in-dict
     :in-dict-prob in-dict-prob
     :ex-dict      *empty-ex-dict*
     :char-markov  char-markov
     :char-int-str char-int-str
     :char-cost-0gram char-cost-0gram)))

(defun read-kkc-dir (dir)
  (labels ((make-path (path)
             (format nil "~A~A" dir path)))
    (read-kkc-paths
     :path-word-int-str (make-path "word/int-str.txt")
     :path-word-1gram   (make-path "word/cost-1gram.tsv")
     :path-word-2gram   (make-path "word/cost-2gram.tsv")
     :path-char-int-str (make-path "char/int-str.txt")
     :path-char-1gram   (make-path "char/cost-1gram.tsv")
     :path-char-2gram   (make-path "char/cost-2gram.tsv")
     :path-in-dict      (make-path "in-dict.tsv"))))
