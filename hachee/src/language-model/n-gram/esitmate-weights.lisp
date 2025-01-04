(defpackage :hachee.language-model.n-gram.estimate-weights
  (:use :cl)
  (:export :estimate-for-2-gram
           :estimate-for-3-gram))
(in-package :hachee.language-model.n-gram.estimate-weights)

(defun improve-iter (weights is-ok improve)
  (loop for count from 0
        for old-weights = weights
            then new-weights
        for new-weights = (funcall improve old-weights)
        when (or (funcall is-ok new-weights old-weights)
                 (<= 30 count))
          return new-weights))

(defun improve-for-2-gram (weights freq corpus BOS EOS)
  (let ((new-w2 0) (new-w1 0) (sum 0))
    (destructuring-bind (w2 w1) weights
      (labels ((update (curr prev)
                 (let ((p1 (* w1 (hachee.language-model.n-gram::conditional-probability
                                  freq curr nil)))
                       (p2 (* w2 (or (hachee.language-model.n-gram::conditional-probability
                                      freq curr (list prev))
                                     0))))
                   (incf new-w1 (/ p1 (+ p1 p2)))
                   (incf new-w2 (/ p2 (+ p1 p2)))
                   (incf sum))))
        (dolist (sentence (hachee.language-model.corpus:corpus-sentence-list corpus))
          (let ((tokens (hachee.language-model.corpus:sentence-tokens sentence))
                (prev BOS))
            (loop for curr in tokens do
              (progn
                (update curr prev)
                (setq prev curr)))
            (update EOS prev)))))
    (setf new-w1 (/ new-w1 sum))
    (setf new-w2 (/ new-w2 sum))
    (let ((tmp (/ (1- (+ new-w1 new-w2)) 2)))
      (list (+ new-w2 tmp) (+ new-w1 tmp)))))

(defun improve-for-3-gram (weights freq corpus BOS EOS)
  (let ((new-w3 0) (new-w2 0) (new-w1 0) (sum 0))
    (destructuring-bind (w3 w2 w1) weights
      (labels ((update (curr prev2 prev1)
                 (let ((p1 (* w1 (hachee.language-model.n-gram::conditional-probability
                                  freq curr nil)))
                       (p2 (* w2 (or (hachee.language-model.n-gram::conditional-probability
                                      freq curr (list prev1))
                                     0)))
                       (p3 (* w3 (or (hachee.language-model.n-gram::conditional-probability
                                      freq curr (list prev2 prev1))
                                     0))))
                   (incf new-w1 (/ p1 (+ p1 p2 p3)))
                   (incf new-w2 (/ p2 (+ p1 p2 p3)))
                   (incf new-w3 (/ p3 (+ p1 p2 p3)))
                   (incf sum))))
        (dolist (sentence (hachee.language-model.corpus:corpus-sentence-list corpus))
          (let ((tokens (hachee.language-model.corpus:sentence-tokens sentence))
                (prev2 BOS)
                (prev1 BOS))
            (loop for curr in tokens do
              (progn
                (update curr prev2 prev1)
                (setq prev2 prev1)
                (setq prev1 curr)))
            (update EOS prev2 prev1)))))
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

(defun estimate-for-2-gram (model-list corpus-list &KEY BOS EOS)
  (let ((size (length corpus-list)))
    (labels ((improve (weights)
               (print weights)
               (let ((new-w2 0) (new-w1 0))
                 (loop for model in model-list
                       for corpus in corpus-list
                       for freq = (hachee.language-model.n-gram::model-freq model) do
                         (destructuring-bind (tmp-w2 tmp-w1)
                             (improve-for-2-gram weights freq corpus BOS EOS)
                           (incf new-w2 tmp-w2)
                           (incf new-w1 tmp-w1)))
                 (list (/ new-w2 size)
                       (/ new-w1 size)))))
      (improve-iter (list 0.8d0 0.2d0) #'is-ok #'improve))))

(defun estimate-for-3-gram (model-list corpus-list &KEY BOS EOS)
  (let ((size (length corpus-list)))
    (labels ((improve (weights)
               (print weights)
               (let ((new-w3 0)
                     (new-w2 0)
                     (new-w1 0))
                 (loop for model in model-list
                       for corpus in corpus-list
                       for freq = (hachee.language-model.n-gram::model-freq model) do
                         (destructuring-bind (tmp-w3 tmp-w2 tmp-w1)
                             (improve-for-3-gram weights freq corpus BOS EOS)
                           (incf new-w3 tmp-w3)
                           (incf new-w2 tmp-w2)
                           (incf new-w1 tmp-w1)))
                 (list (/ new-w3 size)
                       (/ new-w2 size)
                       (/ new-w1 size)))))
      (improve-iter (list 0.7d0 0.2d0 0.1d0) #'is-ok #'improve))))
