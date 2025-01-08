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
        (let ((cls (hachee.language-model.ngram::class-token
                    classifier tok))
              (str (gethash tok to-str-map)))
          (format out "~A~A~A~A~A~%"
                  str #\tab tok #\tab cls))))))

(defun dump-class-model (class-model
                         class-weights-path
                         class-ngram-counts-path
                         class-word-counts-path)
  (with-open-file (out class-weights-path :direction :output)
    (let ((weights (hachee.language-model.ngram:model-weights class-model)))
      (loop for w in weights
            for sep-p = nil then t do
        (format out "~A~A" (if sep-p #\tab "") w))
      (format out "~%")))
  (with-open-file (out class-word-counts-path :direction :output)
    (hachee.language-model.ngram:do-class-model-token-count
        (token count class-model)
      (format out "~A,~A~%" count token)))
  (with-open-file (out class-ngram-counts-path :direction :output)
    (hachee.language-model.ngram:do-model-ngram-count
        (tokens count class-model)
      (format out "~A~{,~A~}~%" count tokens))))

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

(defun dump-unk-model (model unk-ngram-counts-path)
  (with-open-file (out unk-ngram-counts-path :direction :output)
    (hachee.language-model.ngram:do-model-ngram-count
        (tokens count model)
      (format out "~A~{,~A~}~%" count tokens))))

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
   (hachee.language-model.ngram::class-model-classifier
    (kkc-class-model kkc))
   class-vocab-path)
  (dump-class-model (kkc-class-model kkc)
                    class-weights-path
                    class-ngram-counts-path
                    class-word-counts-path)
  (dump-unk-vocabulary (kkc-unk-vocabulary kkc)
                       unk-vocab-path)
  (dump-unk-model (kkc-unk-model kkc)
                  unk-ngram-counts-path)
  (alexandria:write-string-into-file "6878" char-set-size-path)
  (values))
