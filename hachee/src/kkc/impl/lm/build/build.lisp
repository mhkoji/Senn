(defpackage :hachee.kkc.impl.lm.build
  (:use :cl)
  (:export :build-vocabulary
           :build-vocabulary-with-unk
           :extend-existing-vocabulary
           :estimate-2gram-weights
           :build-word-dictionary
           :train-ngram-model
           :build-unknown-word-vocabulary
           :build-unknown-word-ngram-model
           :add-to-word-dictionary-from-resources
           :build-tankan-dictionary
           :build-classifier)
  (:local-nicknames (:voc :hachee.language-model.vocabulary))
  (:local-nicknames (:unit :hachee.kkc.impl.lm.unit))
  (:local-nicknames (:file :hachee.kkc.impl.lm.build.file))
  (:local-nicknames (:freq :hachee.language-model.ngram.freq))
  (:local-nicknames (:dict :hachee.kkc.impl.lm.dictionary))
  (:local-nicknames (:ngram :hachee.language-model.ngram))
  (:local-nicknames (:estimate-weights
                     :hachee.language-model.ngram.estimate-weights)))
(in-package :hachee.kkc.impl.lm.build)

(defun build-vocabulary (pathnames)
  (let ((vocab (voc:make-vocabulary))
        (word-key->freq (make-hash-table :test #'equal)))
    (dolist (pathname pathnames)
      (file:do-lines (line pathname)
        (dolist (word (file:line-units line))
          (incf (gethash (unit:unit->key word) word-key->freq 0)))))
    (let ((skipped-for-UNK-p nil))
      (maphash (lambda (word-key count)
                 (if (or (< 1 count) skipped-for-UNK-p)
                     (voc:add-new vocab word-key)
                     (setq skipped-for-UNK-p t)))
               word-key->freq))
    vocab))

(defun build-vocabulary-with-unk (pathnames &key (overlap 2))
  (assert (<= overlap (length pathnames)))
  (format *error-output* "Building initial vocabulary ...~%")
  (let ((vocab (voc:make-vocabulary))
        (word-key->freq (make-hash-table :test #'equal)))
    (dolist (pathname pathnames)
      (let ((curr-words (make-hash-table :test #'equal)))
        (file:do-lines (line pathname)
          (dolist (word (file:line-units line))
            (setf (gethash (unit:unit->key word) curr-words) word)))
        (maphash (lambda (word-key word)
                   (let ((freq (incf (gethash word-key word-key->freq 0))))
                     (when (<= overlap freq)
                       (voc:add-new vocab (unit:unit->key word)))))
                 curr-words)))
    vocab))

(labels ((build-freq (pathnames vocabulary n BOS EOS)
           (format *error-output* "Building freq ...~%")
           (let ((freq (freq:make-freq)))
             (freq:with-add-counts
                 (add-counts freq :n n :BOS BOS :EOS EOS)
               (dolist (pathname pathnames)
                 (file:do-sentence
                     (sentence pathname vocabulary)
                   (add-counts (ngram:sentence-tokens sentence)))))
             freq))
         (build-corpus (p vocabulary)
           (let ((sentence-list nil))
             (file:do-sentence (s p vocabulary)
               (push s sentence-list))
             (estimate-weights:make-corpus
              :sentence-list (nreverse sentence-list)))))
  (defun estimate-2gram-weights (pathnames vocabulary)
    (let ((BOS (voc:to-int vocabulary voc:+BOS+))
          (EOS (voc:to-int vocabulary voc:+EOS+)))
      (let ((items (loop for i from 0
                         for corpus-pathname in pathnames
                         for freq-pathnames = (loop for p in pathnames
                                                    for j from 0
                                                    when (/= j i) collect p)
                         collect (estimate-weights:make-cross-item
                                  :freq (build-freq freq-pathnames
                                                    vocabulary 2 BOS EOS)
                                  :corpus (build-corpus corpus-pathname
                                                        vocabulary)))))
        (estimate-weights:estimate-2gram items BOS EOS)))))

(defun extend-existing-vocabulary (vocabulary
                                   trusted-word-dictionary
                                   pathnames-inaccurately-segmented)
  (format *error-output* "Extending vocabulary ...~%")
  (dolist (pathname pathnames-inaccurately-segmented)
    (file:do-lines (line pathname)
      (dolist (unit (file:line-units line))
        (when (dict:contains-p trusted-word-dictionary unit)
          (voc:add-new vocabulary (unit:unit->key unit))))))
  vocabulary)

(defun build-word-dictionary (pathnames vocabulary)
  (format *error-output* "Building dictionary ...~%")
  (let ((dict (dict:make-dictionary)))
    (dolist (pathname pathnames)
      (file:do-lines (line pathname)
        (dolist (unit (file:line-units line))
          (if (voc:to-int-or-nil vocabulary (unit:unit->key unit))
              (dict:add-entry dict unit hachee.kkc.origin:+vocabulary+)
              (dict:add-entry dict unit hachee.kkc.origin:+corpus+)))))
    dict))

(defun train-ngram-model (model pathnames vocabulary)
  (format *error-output* "Building ngram model ...~%")
  (ngram:with-model-add-counts (model-add-counts model
                                :BOS (voc:to-int vocabulary voc:+BOS+)
                                :EOS (voc:to-int vocabulary voc:+EOS+))
    (dolist (pathname pathnames)
      (file:do-sentence (sentence pathname vocabulary)
        (model-add-counts sentence))))
  model)

(defun build-unknown-word-vocabulary (pathnames vocabulary &key (overlap 2))
  (format *error-output* "Building unknown word vocabulary ...~%")
  (let ((key->freq (make-hash-table :test #'equal))
        (pron-vocab (voc:make-vocabulary)))
    (dolist (pathname pathnames)
      (let ((curr-prons (make-hash-table :test #'equal)))
        (file:do-lines (line pathname)
          (dolist (unit (file:line-units line))
            (when (not (voc:to-int-or-nil vocabulary (unit:unit->key unit)))
              (dolist (pron-unit (unit:unit->pron-units unit))
                (setf (gethash (unit:unit->key pron-unit) curr-prons)
                      pron-unit)))))
        (maphash (lambda (key pron-unit)
                   (let ((freq (incf (gethash key key->freq 0))))
                     (when (<= overlap freq)
                       (voc:add-new pron-vocab (unit:unit->key pron-unit)))))
                 curr-prons)))
    pron-vocab))

(defun pron->sentence (pron unknown-word-char-vocabulary)
  (ngram:make-sentence
   :tokens (loop for ch across pron
                 for unit = (unit:make-unit :form (string ch)
                                            :pron (string ch))
                 collect (voc:to-int-or-unk unknown-word-char-vocabulary
                                            (unit:unit->key unit)))))

(defun build-unknown-word-ngram-model (pathnames
                                       vocabulary
                                       unknown-word-vocabulary)
  (format *error-output* "Building unknown word ngram model ...~%")
  (let ((model (make-instance 'ngram:model)))
    (ngram:with-model-add-counts
        (model-add-counts model
         :BOS (voc:to-int unknown-word-vocabulary voc:+BOS+)
         :EOS (voc:to-int unknown-word-vocabulary voc:+EOS+))
      (dolist (pathname pathnames)
        (file:do-lines (line pathname)
          (dolist (unit (file:line-units line))
            (when (not (voc:to-int-or-nil vocabulary (unit:unit->key unit)))
              (model-add-counts (pron->sentence
                                 (unit:unit-pron unit)
                                 unknown-word-vocabulary)))))))
    model))

(defun add-to-word-dictionary-from-resources (dict pathnames)
  (format *error-output* "Building word dictionary ...~%")
  (dolist (pathname pathnames)
    (with-open-file (in pathname)
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (form part pron) (cl-ppcre:split "/" line)
          (declare (ignore part))
          (let ((unit (unit:make-unit :form form :pron pron)))
            (dict:add-entry dict unit hachee.kkc.origin:+resource+))))))
  dict)

(defun build-tankan-dictionary (dict pathnames)
  (dolist (pathname pathnames)
    (with-open-file (in pathname)
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (form pron) (cl-ppcre:split "/" line)
          (let ((char-unit (unit:make-unit :form form :pron pron)))
            (dict:add-entry dict char-unit hachee.kkc.origin:+tankan+))))))
  dict)

(defun build-classifier (class-token-to-word-file-path vocabulary)
  (let ((to-class-map (make-hash-table :test #'equal)))
    (with-open-file (in class-token-to-word-file-path
                        :direction :input
                        :external-format :utf-8)
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (class-token-string form-pron-string)
            (cl-ppcre:split " " line)
          (let ((class-token (parse-integer class-token-string)))
            (cond ((string= form-pron-string "UT")
                   (setf (gethash (voc:to-int vocabulary voc:+UNK+)
                                  to-class-map)
                         class-token))
                  ((string= form-pron-string "BT")
                   (setf (gethash (voc:to-int vocabulary voc:+BOS+)
                                  to-class-map)
                         class-token))
                  (t
                   (let* ((forms
                           (mapcar (lambda (x)
                                     (first (cl-ppcre:split "/" x)))
                                   (cl-ppcre:split "-" form-pron-string)))
                          (prons
                           (mapcar (lambda (x)
                                     (second (cl-ppcre:split "/" x)))
                                   (cl-ppcre:split "-" form-pron-string)))
                          (token
                           (voc:to-int vocabulary
                                       (unit:unit->key
                                        (unit:make-unit
                                         :form (apply #'concatenate
                                                      'string forms)
                                         :pron (apply #'concatenate
                                                      'string prons))))))
                     (setf (gethash token to-class-map) class-token))))))))
    (let ((EOS-class-token (voc::vocabulary-size vocabulary)))
      (setf (gethash (voc:to-int vocabulary voc:+EOS+) to-class-map)
            EOS-class-token))
    (ngram:make-classifier :to-class-map to-class-map)))
