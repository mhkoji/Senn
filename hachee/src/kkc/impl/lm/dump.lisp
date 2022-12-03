(defpackage :hachee.kkc.impl.lm.dump
  (:use :cl))
(in-package :hachee.kkc.impl.lm.dump)

(defun dump-vocabulary (vocab classifier out-path)
  (let ((vocab-size (hachee.language-model.vocabulary::vocabulary-size
                     vocab))
        (to-int-map (hachee.language-model.vocabulary::vocabulary-to-int-map
                     vocab))
        (to-str-map (make-hash-table :test #'equal)))
    (maphash (lambda (str int)
               (assert (null (gethash int to-str-map)))
               (setf (gethash int to-str-map) str))
             to-int-map)
    (with-open-file (out out-path :direction :output)
      (dotimes (tok vocab-size)
        (let ((cls (hachee.language-model.n-gram::class-token
                    classifier tok))
              (str (gethash tok to-str-map)))
          (format out "~A~A~A~A~A~%"
                  tok #\tab str #\tab cls))))))

(defun dump-class-model (class-model vocab class-trans-path token-count-path)
  (with-open-file (out class-trans-path :direction :output)
    (let* ((to-class-map
            (hachee.language-model.n-gram::classifier-to-class-map
             (hachee.language-model.n-gram::class-model-classifier
              class-model)))
           (class-list
            (remove-duplicates
             (sort (loop for class being the hash-value of to-class-map
                         collect class)
                   #'<)
             :test #'=)))
      (dolist (prev class-list)
        (let ((hist (list prev)))
          (dolist (next class-list)
            (let ((prob
                   (hachee.language-model.n-gram::interpolated-probability
                    class-model next hist)))
              (when (/= prob 0)
                (format out "~A~A~A~A~A~%"
                        prev #\tab next #\tab prob))))))))
  (with-open-file (out token-count-path :direction :output)
    (let ((vocab-size (hachee.language-model.vocabulary::vocabulary-size
                       vocab))
          (token-freq (hachee.language-model.n-gram::class-model-token-freq
                       class-model))
          (bos-token (hachee.language-model.vocabulary:to-int
                      vocab hachee.language-model.vocabulary:+BOS+)))
      (dotimes (token vocab-size)
        (let ((count (if (= token bos-token)
                         0
                         (gethash token token-freq))))
          (assert count)
          (format out "~A~A~A~%" token #\tab count))))))
