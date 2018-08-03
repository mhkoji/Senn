(defpackage :hachee.t.scenario.chu-liu-edmonds
  (:use :cl)
  (:export :solve1 :solve2 :solve3))
(in-package :hachee.t.scenario.chu-liu-edmonds)


(defmacro solve1 (&key test)
  `(let ((N 4)
         (s (make-hash-table :test #'equal)))
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 0 1)  9)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 0 2) 10)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 0 3)  9)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 1 2) 20)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 1 3) 11)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 2 1) 30)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 2 3) 30)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 3 1) 11)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 3 2)  0)
     (,test (null (set-difference
                   (hachee.algorithm.chu-liu-edmonds:solve
                    N
                    s
                    (loop for i from 0 below N collect i))
                   '((2 1) (2 3) (0 2))
                   :test #'equal)))))


(defmacro solve2 (&key test)
  `(let ((N 8)
         (s (make-hash-table :test #'equal)))
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 0 1)  1)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 0 2)  3)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 0 3)  1)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 1 2)  2)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 1 4)  5)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 2 5) 10)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 3 2)  1)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 3 7)  8)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 4 2) 10)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 5 4) 10)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 5 7) 20)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 6 5) 20)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 7 6) 20)
     (,test (null (set-difference
                   (hachee.algorithm.chu-liu-edmonds:solve
                    N
                    s
                    (loop for i from 0 below N collect i))
                   '((6 5) (7 6) (0 1) (0 3) (3 7) (5 4) (4 2))
                   :test #'equal)))))

(defmacro solve3 (&key test)
  `(let ((N 4)
         (s (make-hash-table :test #'equal)))
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 0 1)   3)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 0 2) 1.5)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 0 3) 1.8)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 1 2) 4.3)
     (setf (hachee.algorithm.chu-liu-edmonds:get-score s 2 3) 2.2)
     (,test (null (set-difference
                   (hachee.algorithm.chu-liu-edmonds:solve
                    N
                    s
                    (loop for i from 0 below N collect i))
                   '((0 1) (1 2) (2 3))
                   :test #'equal)))))
