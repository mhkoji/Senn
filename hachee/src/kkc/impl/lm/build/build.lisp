(defpackage :hachee.kkc.impl.lm.build
  (:use :cl)
  (:import-from :hachee.language-model.vocabulary
                :add-new
                :to-int
                :to-int-or-nil
                :to-int-or-unk)
  (:import-from :hachee.kkc.impl.lm.unit
                :make-unit
                :unit-form
                :unit-pron
                :unit->key
                :unit->pron-units)
  (:export :build-vocabulary
           :build-vocabulary-with-unk
           :extend-existing-vocabulary
           :build-word-dictionary
           :train-n-gram-model
           :build-unknown-word-vocabulary
           :build-unknown-word-n-gram-model
           :add-to-word-dictionary-from-resources
           :build-tankan-dictionary
           :build-classifier))
(in-package :hachee.kkc.impl.lm.build)

(defun to-token-sentence (file-sentence vocabulary)
  (hachee.language-model.n-gram:make-sentence
   :tokens
   (mapcar (lambda (u)
             (to-int-or-unk vocabulary (unit->key u)))
           (hachee.kkc.impl.lm.build.file:sentence-units file-sentence))))

(defun build-vocabulary (pathnames)
  (let ((vocab (hachee.language-model.vocabulary:make-vocabulary))
        (word-key->freq (make-hash-table :test #'equal)))
    (dolist (pathname pathnames)
      (dolist (sentence (hachee.kkc.impl.lm.build.file:file->sentences
                         pathname))
        (dolist (word (hachee.kkc.impl.lm.build.file:sentence-units
                       sentence))
          (incf (gethash (unit->key word) word-key->freq 0)))))
    (let ((skipped-for-UNK-p nil))
      (maphash (lambda (word-key count)
                 (if (or (< 1 count) skipped-for-UNK-p)
                     (add-new vocab word-key)
                     (setq skipped-for-UNK-p t)))
               word-key->freq))
    vocab))

(defun build-vocabulary-with-unk (pathnames &key (overlap 2))
  (assert (<= overlap (length pathnames)))
  (format *error-output* "Building initial vocabulary ...~%")
  (let ((vocab (hachee.language-model.vocabulary:make-vocabulary))
        (word-key->freq (make-hash-table :test #'equal)))
    (dolist (pathname pathnames)
      (let ((curr-words (make-hash-table :test #'equal)))
        (dolist (sentence (hachee.kkc.impl.lm.build.file:file->sentences
                           pathname))
          (dolist (word (hachee.kkc.impl.lm.build.file:sentence-units
                         sentence))
            (setf (gethash (unit->key word) curr-words) word)))
        (maphash (lambda (word-key word)
                   (let ((freq (incf (gethash word-key word-key->freq 0))))
                     (when (<= overlap freq)
                       (add-new vocab (unit->key word)))))
                 curr-words)))
    vocab))

(defun extend-existing-vocabulary (vocabulary
                                   trusted-word-dictionary
                                   pathnames-inaccurately-segmented)
  (format *error-output* "Extending vocabulary ...~%")
  (dolist (pathname pathnames-inaccurately-segmented)
    (dolist (sentence (hachee.kkc.impl.lm.build.file:file->sentences
                       pathname))
      (dolist (unit (hachee.kkc.impl.lm.build.file:sentence-units sentence))
        (when (hachee.kkc.impl.lm.dictionary:contains-p
               trusted-word-dictionary unit)
          (add-new vocabulary (unit->key unit))))))
  vocabulary)

(defun build-word-dictionary (pathnames vocabulary)
  (format *error-output* "Building dictionary ...~%")
  (let ((dict (hachee.kkc.impl.lm.dictionary:make-dictionary)))
    (dolist (pathname pathnames)
      (dolist (sentence (hachee.kkc.impl.lm.build.file:file->sentences
                         pathname))
        (dolist (unit (hachee.kkc.impl.lm.build.file:sentence-units
                       sentence))
          (if (to-int-or-nil vocabulary (unit->key unit))
              (hachee.kkc.impl.lm.dictionary:add-entry
               dict unit hachee.kkc.origin:+vocabulary+)
              (hachee.kkc.impl.lm.dictionary:add-entry
               dict unit hachee.kkc.origin:+corpus+)))))
    dict))

(defun train-n-gram-model (model pathnames vocabulary)
  (format *error-output* "Building n-gram model ...~%")
  (let ((BOS (to-int vocabulary hachee.language-model.vocabulary:+BOS+))
        (EOS (to-int vocabulary hachee.language-model.vocabulary:+EOS+)))
    (dolist (pathname pathnames)
      (hachee.kkc.impl.lm.build.file:with-sentence-reader
          (next-file-sentence pathname)
        (labels ((next-token-sentence ()
                   (let ((file-sentence (next-file-sentence)))
                     (when file-sentence
                       (to-token-sentence file-sentence vocabulary)))))
          (hachee.language-model.n-gram:train
           model #'next-token-sentence :BOS BOS :EOS EOS)))))
  model)

(defun build-unknown-word-vocabulary (pathnames vocabulary &key (overlap 2))
  (format *error-output* "Building unknown word vocabulary ...~%")
  (let ((key->freq (make-hash-table :test #'equal))
        (pron-vocab (hachee.language-model.vocabulary:make-vocabulary)))
    (dolist (pathname pathnames)
      (let ((curr-prons (make-hash-table :test #'equal)))
        (dolist (sentence (hachee.kkc.impl.lm.build.file:file->sentences
                           pathname))
          (dolist (unit (hachee.kkc.impl.lm.build.file:sentence-units
                         sentence))
            (when (not (to-int-or-nil vocabulary (unit->key unit)))
              (dolist (pron-unit (unit->pron-units unit))
                (setf (gethash (unit->key pron-unit) curr-prons)
                      pron-unit)))))
        (maphash (lambda (key pron-unit)
                   (let ((freq (incf (gethash key key->freq 0))))
                     (when (<= overlap freq)
                       (add-new pron-vocab (unit->key pron-unit)))))
                 curr-prons)))
    pron-vocab))

(defun pron->sentence (pron unknown-word-char-vocabulary)
  (hachee.language-model.n-gram:make-sentence
   :tokens (loop for ch across pron
                 for unit = (hachee.kkc.impl.lm.unit:make-unit
                             :form (string ch)
                             :pron (string ch))
                 collect (to-int-or-unk
                          unknown-word-char-vocabulary
                          (hachee.kkc.impl.lm.unit:unit->key unit)))))

(defun build-unknown-word-n-gram-model (pathnames
                                        vocabulary
                                        unknown-word-vocabulary)
  (format *error-output* "Building unknown word n-gram model ...~%")
  (let ((BOS (to-int unknown-word-vocabulary
                     hachee.language-model.vocabulary:+BOS+))
        (EOS (to-int unknown-word-vocabulary
                     hachee.language-model.vocabulary:+EOS+))
        (model (make-instance 'hachee.language-model.n-gram:model)))
  (dolist (pathname pathnames)
    (dolist (file-sentence (hachee.kkc.impl.lm.build.file:file->sentences
                            pathname))
      (dolist (unit (hachee.kkc.impl.lm.build.file:sentence-units
                     file-sentence))
        (when (not (to-int-or-nil vocabulary (unit->key unit)))
          (let ((sentence (pron->sentence (unit-pron unit)
                                          unknown-word-vocabulary)))
            (hachee.language-model.n-gram:train model (list sentence)
                                                :BOS BOS
                                                :EOS EOS))))))
    model))

(defun add-to-word-dictionary-from-resources (dict pathnames)
  (format *error-output* "Building word dictionary ...~%")
  (dolist (pathname pathnames)
    (with-open-file (in pathname)
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (form part pron) (cl-ppcre:split "/" line)
          (declare (ignore part))
          (let ((unit (make-unit :form form :pron pron)))
            (hachee.kkc.impl.lm.dictionary:add-entry
             dict unit hachee.kkc.origin:+resource+))))))
  dict)

(defun build-tankan-dictionary (dict pathnames)
  (dolist (pathname pathnames)
    (with-open-file (in pathname)
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (form pron) (cl-ppcre:split "/" line)
          (let ((char-unit (make-unit :form form
                                      :pron pron)))
            (hachee.kkc.impl.lm.dictionary:add-entry
             dict char-unit hachee.kkc.origin:+tankan+))))))
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
                   (setf (gethash (to-int
                                   vocabulary
                                   hachee.language-model.vocabulary:+UNK+)
                                  to-class-map)
                         class-token))
                  ((string= form-pron-string "BT")
                   (setf (gethash (to-int
                                   vocabulary
                                   hachee.language-model.vocabulary:+BOS+)
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
                           (to-int vocabulary
                                   (unit->key
                                    (make-unit
                                     :form (apply #'concatenate
                                                  'string forms)
                                     :pron (apply #'concatenate
                                                  'string prons))))))
                     (setf (gethash token to-class-map) class-token))))))))
    (let ((EOS-class-token
           (hachee.language-model.vocabulary::vocabulary-size vocabulary)))
      (setf (gethash (to-int vocabulary
                             hachee.language-model.vocabulary:+EOS+)
                     to-class-map)
            EOS-class-token))
    (hachee.language-model.n-gram:make-classifier
     :to-class-map to-class-map)))
