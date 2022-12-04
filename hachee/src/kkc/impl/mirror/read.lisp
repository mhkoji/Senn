;; in-dict, int-str, markovは読み込み処理が似ているのでこのパッケージにまとめた。
(defpackage :hachee.kkc.impl.mirror.read
  (:use :cl)
  (:export :read-kkc-dir
           :read-kkc-paths)
  (:local-nicknames (:int-str :hachee.kkc.impl.mirror.int-str)))
(in-package :hachee.kkc.impl.mirror.read)

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
            (let ((entry (hachee.kkc.impl.mirror.in-dict:make-entry
                          :form form
                          :cost (parse-integer cost)
                          :token (int-str:to-int int-str form))))
              (push entry (gethash pron hash)))))))
    (hachee.kkc.impl.mirror.in-dict:make-in-dict :hash hash)))

;;;

(defun read-cost-1gram (path int-str)
  (let ((array (make-array (int-str:int-str-size int-str))))
    (with-open-file (stream path :external-format :utf-8)
      (loop for line = (read-line stream nil nil) while line do
        (progn
          (destructuring-bind (str cost-str) (cl-ppcre:split "\\t" line)
            (let ((int (int-str:to-int int-str str))
                  (cost (parse-integer cost-str)))
              (when (= int int-str:+UT+)
                ;; strが"UT"以外の未知語の場合、int-str:to-intがUTを返して
                ;; 本来のUTのcostが上書きされてしまうのを防ぐ。
                ;; そもそもstrが"UT"以外の未知語になる場合は、
                ;; 読み込むデータが不整合になっている。
                (assert (string= str "UT")))
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
              (when (= i-int int-str:+UT+)
                (assert (string= i-str "UT")))
              (when (= j-int int-str:+UT+)
                (assert (string= j-str "UT")))
              (setf (gethash (list i-int j-int) hash) cost))))))
    hash))

(defun read-markov (1gram-path 2gram-path int-str)
  (hachee.kkc.impl.mirror:make-markov
   :cost-1gram (read-cost-1gram 1gram-path int-str)
   :cost-2gram (read-cost-2gram 2gram-path int-str)))

;;;

(defun read-task (path-cost-1gram
                  path-cost-2gram
                  path-in-dict
                  path-coeffs
                  path-int-str)
  (let ((int-str (read-int-str path-int-str))
        (coeffs (with-open-file (in path-coeffs)
                  (read in))))
    (hachee.kkc.impl.mirror:build-task
     :markov (read-markov path-cost-1gram
                          path-cost-2gram
                          int-str)
     :in-dict (read-in-dict path-in-dict int-str)
     :coeffs coeffs)))

;;;

(defun read-kkc-paths (&key path-word-int-str
                            path-word-1gram
                            path-word-2gram
                            path-char-int-str
                            path-char-1gram
                            path-char-2gram
                            path-in-dict
                            path-task-int-str
                            path-task-1gram
                            path-task-2gram
                            path-task-in-dict
                            path-task-coeffs)
  (let* ((word-int-str (read-int-str path-word-int-str))
         (word-markov  (read-markov path-word-1gram
                                    path-word-2gram
                                    word-int-str))
         (in-dict      (read-in-dict path-in-dict word-int-str))
         (char-int-str (read-int-str path-char-int-str))
         (char-markov  (read-markov path-char-1gram
                                    path-char-2gram
                                    char-int-str))
         (task         (read-task path-task-1gram
                                  path-task-2gram
                                  path-task-in-dict
                                  path-task-coeffs
                                  path-task-int-str)))
    (hachee.kkc.impl.mirror:build-kkc
     :word-markov  word-markov
     :in-dict      in-dict
     :char-int-str char-int-str
     :char-markov  char-markov
     :task         task)))

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
     :path-in-dict      (make-path "in-dict.tsv")
     :path-task-int-str (make-path "task/int-str.txt")
     :path-task-1gram   (make-path "task/cost-1gram.tsv")
     :path-task-2gram   (make-path "task/cost-2gram.tsv")
     :path-task-in-dict (make-path "task/in-dict.tsv")
     :path-task-coeffs  (make-path "task/coeffs.txt"))))
