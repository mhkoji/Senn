(defpackage :hachee.algorithm.longest-common-subsequence
  (:use :cl)
  (:export :compute-length))
(in-package :hachee.algorithm.longest-common-subsequence)

(defun compute-length-non-empty (str1 str2)
  (let* ((str1-len (length str1))
         (str2-len (length str2))
         (table (make-array (list str1-len str2-len))))
    (let ((char2 (char str2 0)))
      (dotimes (i1 str1-len)
        (if (char= char2 (char str1 i1))
            (setf (aref table i1 0) 1)
            (setf (aref table i1 0) 0))))
    (let ((char1 (char str1 0)))
      (dotimes (i2 str2-len)
        (if (char= char1 (char str2 i2))
            (setf (aref table 0 i2) 1)
            (setf (aref table 0 i2) 0))))
    (loop for i1 from 1 below str1-len do
      (loop for i2 from 1 below str2-len do
        (setf (aref table i1 i2)
              (if (char= (char str1 i1)
                         (char str2 i2))
                  (1+ (aref table (1- i1) (1- i2)))
                  (max (aref table (1- i1) i2)
                       (aref table i1 (1- i2)))))))
    (aref table (1- str1-len) (1- str2-len))))

(defun compute-length (str1 str2)
  (if (or (string= str1 "") (string= str2 ""))
      0
      (compute-length-non-empty str1 str2)))

(progn
  (assert (= (compute-length "0abcd0" "00ab1abc0")
             4)))
