(defpackage :hachee.kkc.impl.lm.dump
  (:use :cl)
  (:export :kkc-class-vocabulary
           :kkc-class-model
           :kkc-unk-vocabulary
           :kkc-unk-model
           :execute))
(in-package :hachee.kkc.impl.lm.dump)

(defgeneric kkc-class-vocabulary (kkc))
(defgeneric kkc-class-model (kkc))
(defgeneric kkc-unk-vocabulary (kkc))
(defgeneric kkc-unk-model (kkc))

(defun dump-class-vocabulary (vocab classifier out-path)
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
                  str #\tab tok #\tab cls))))))

(defun dump-class-model (class-model
                         class-weights-path
                         class-ngram-counts-path
                         class-word-counts-path)
  (let ((weights (hachee.language-model.n-gram::model-weights class-model)))
    (assert (= (length weights) 2))
    (with-open-file (out class-weights-path :direction :output)
      (format out "~A~A~A~%" (nth 0 weights) #\tab (nth 1 weights))))
  (let* ((token-freq (hachee.language-model.n-gram::class-model-token-freq
                      class-model))
         (token-list (sort (loop for token being the hash-key of token-freq
                                 collect token)
                           #'<)))
    (with-open-file (out class-word-counts-path :direction :output)
      (dolist (token token-list)
        (let ((count (gethash token token-freq)))
          (assert count)
          (format out "~A,~A~%" count token)))))
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
    (with-open-file (out class-ngram-counts-path :direction :output)
      ;; 0-gram
      (let ((count (hachee.language-model.n-gram:get-count
                    class-model nil)))
        (format out "~A~%" count))
      ;; 1-gram
      (dolist (class class-list)
        (let ((count (hachee.language-model.n-gram:get-count
                      class-model (list class))))
          (assert count)
          (format out "~A,~A~%" count class)))
      ;; 2-gram
      (dolist (prev class-list)
        (dolist (next class-list)
          (let ((count (hachee.language-model.n-gram:get-count
                        class-model (list prev next))))
            (when count
              (format out "~A,~A,~A~%" count prev next))))))))

(defun dump-unk-vocabulary (vocab out-path)
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
        (let ((str (hachee.kkc.impl.lm.unit:unit-form
                    (gethash tok to-str-map))))
          (format out "~A~A~A~%" tok #\tab str))))))

(defun dump-unk-model (model vocab unk-ngram-counts-path)
  (with-open-file (out unk-ngram-counts-path :direction :output)
    (let ((0-gram-count (hachee.language-model.n-gram:get-count model nil)))
      (if (not 0-gram-count)
          (format out "0~%")
          (let ((vocab-size
                 (hachee.language-model.vocabulary::vocabulary-size vocab)))
            ;; 0-gram
            (format out "~A~%" 0-gram-count)
            ;; 1-gram
            (dotimes (token vocab-size)
              (let ((count (hachee.language-model.n-gram:get-count
                            model (list token))))
                (assert count)
                (format out "~A,~A~%" count token)))
            ;; 2-gram
            (dotimes (prev vocab-size)
              (dotimes (next vocab-size)
                (let ((count (hachee.language-model.n-gram:get-count
                              model (list prev next))))
                  (when count
                    (format out "~A,~A,~A~%" count prev next))))))))))

(defun execute (kkc
                class-vocab-path
                class-weights-path
                class-ngram-counts-path
                class-word-counts-path
                unk-vocab-path
                unk-ngram-counts-path
                char-set-size-path)
  (dump-class-vocabulary
   (kkc-class-vocabulary kkc)
   (hachee.language-model.n-gram::class-model-classifier
    (kkc-class-model kkc))
   class-vocab-path)
  (dump-class-model (kkc-class-model kkc)
                    class-weights-path
                    class-ngram-counts-path
                    class-word-counts-path)
  (dump-unk-vocabulary (kkc-unk-vocabulary kkc)
                       unk-vocab-path)
  (dump-unk-model (kkc-unk-model kkc)
                  (kkc-unk-vocabulary kkc)
                  unk-ngram-counts-path)
  (alexandria:write-string-into-file "6878" char-set-size-path)
  (values))
