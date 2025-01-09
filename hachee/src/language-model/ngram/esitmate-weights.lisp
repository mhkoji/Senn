(defpackage :hachee.language-model.ngram.estimate-weights
  (:use :cl)
  (:export :make-corpus
           :make-cross-item
           :estimate-2gram
           :estimate-3gram))
(in-package :hachee.language-model.ngram.estimate-weights)

(defstruct corpus sentence-list)

(defstruct cross-item freq corpus)

(defun improve-iter (weights is-ok improve)
  (loop for count from 0
        for old-weights = weights
            then new-weights
        for new-weights = (funcall improve old-weights)
        when (or (funcall is-ok new-weights old-weights)
                 (<= 30 count))
          return new-weights))

(defun improve-for-item-2gram (weights item BOS EOS)
  (let ((freq (cross-item-freq item))
        (corpus (cross-item-corpus item))
        (new-w2 0) (new-w1 0) (sum 0))
    (labels ((update (curr prev)
               (destructuring-bind (p2 p1)
                   (hachee.language-model.ngram.probability:weighted-list
                    freq weights curr (list prev))
                 (incf new-w1 (/ p1 (+ p1 p2)))
                 (incf new-w2 (/ p2 (+ p1 p2)))
                 (incf sum))))
      (dolist (sentence (corpus-sentence-list corpus))
        (let ((prev BOS))
          (dolist (curr (hachee.language-model.ngram:sentence-tokens
                         sentence))
            (update curr prev)
            (setq prev curr))
          (update EOS prev))))
    (setf new-w1 (/ new-w1 sum))
    (setf new-w2 (/ new-w2 sum))
    (let ((tmp (/ (1- (+ new-w1 new-w2)) 2)))
      (list (+ new-w2 tmp) (+ new-w1 tmp)))))

(defun improve-for-item-3gram (weights item BOS EOS)
  (let ((freq (cross-item-freq item))
        (corpus (cross-item-corpus item))
        (new-w3 0) (new-w2 0) (new-w1 0) (sum 0))
    (labels ((update (curr prev2 prev1)
               (destructuring-bind (p3 p2 p1)
                   (hachee.language-model.ngram.probability:weighted-list
                    freq weights curr (list prev2 prev1))
                 (incf new-w1 (/ p1 (+ p1 p2 p3)))
                 (incf new-w2 (/ p2 (+ p1 p2 p3)))
                 (incf new-w3 (/ p3 (+ p1 p2 p3)))
                 (incf sum))))
      (dolist (sentence (corpus-sentence-list corpus))
        (let ((prev2 BOS)
              (prev1 BOS))
          (dolist (curr (hachee.language-model.ngram:sentence-tokens
                         sentence))
            (update curr prev2 prev1)
            (setq prev2 prev1)
            (setq prev1 curr))
          (update EOS prev2 prev1))))
    (setf new-w1 (/ new-w1 sum))
    (setf new-w2 (/ new-w2 sum))
    (setf new-w3 (/ new-w3 sum))
    (let ((tmp (/ (1- (+ new-w1 new-w2 new-w3)) 3)))
      (list (+ new-w3 tmp)
            (+ new-w2 tmp)
            (+ new-w1 tmp)))))

(defun is-ok (new-weights old-weights)
  (< (abs (- (car new-weights)
             (car old-weights)))
     0.0001d0))

(defun estimate-2gram (items BOS EOS)
  (let ((size (length items)))
    (labels ((improve (weights)
               (print weights)
               (let ((new-w2 0) (new-w1 0))
                 (loop for item in items
                       for (w2 w1) = (improve-for-item-2gram
                                      weights item BOS EOS)
                       do (progn (incf new-w2 w2)
                                 (incf new-w1 w1)))
                 (list (/ new-w2 size)
                       (/ new-w1 size)))))
      (improve-iter (list 0.8d0 0.2d0) #'is-ok #'improve))))

(defun estimate-3gram (items BOS EOS)
  (let ((size (length items)))
    (labels ((improve (weights)
               (print weights)
               (let ((new-w3 0) (new-w2 0) (new-w1 0))
                 (loop for item in items
                       for (w3 w2 w1) = (improve-for-item-3gram
                                         weights item BOS EOS)
                       do (progn (incf new-w3 w3)
                                 (incf new-w2 w2)
                                 (incf new-w1 w1)))
                 (list (/ new-w3 size)
                       (/ new-w2 size)
                       (/ new-w1 size)))))
      (improve-iter (list 0.7d0 0.2d0 0.1d0) #'is-ok #'improve))))
