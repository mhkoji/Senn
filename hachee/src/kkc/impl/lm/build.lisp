(defpackage :hachee.kkc.impl.lm.build
  (:use :cl)
  (:export :build-kkc
           :build-kkc-simple)
  (:local-nicknames (:voc :hachee.language-model.vocabulary))
  (:local-nicknames (:unit :hachee.kkc.impl.lm.unit))
  (:local-nicknames (:freq :hachee.language-model.ngram.freq))
  (:local-nicknames (:dict :hachee.kkc.impl.lm.dictionary))
  (:local-nicknames (:ngram :hachee.language-model.ngram))
  (:local-nicknames (:corpus :hachee.corpus))
  (:local-nicknames (:estimate-weights
                     :hachee.language-model.ngram.estimate-weights)))
(in-package :hachee.kkc.impl.lm.build)

(defun pron->sentence (pron unknown-word-char-vocabulary)
  (labels ((ch->token (ch)
             (let ((unit (unit:make-unit :form (string ch)
                                         :pron (string ch))))
               (voc:to-int-or-unk unknown-word-char-vocabulary
                                  (unit:unit->key unit)))))
    (ngram:make-sentence
     :tokens (loop for ch across pron collect (ch->token ch)))))

(defun line-units (line)
  (corpus:line-units line (lambda (f p)
                            (unit:make-unit :form f :pron p))))


(defun line->sentence (line vocabulary)
  (ngram:make-sentence
   :tokens (mapcar (lambda (u)
                     (voc:to-int-or-unk vocabulary (unit:unit->key u)))
                   (line-units line))))

(defmacro do-sentences ((sentence filename vocabulary) &body body)
  `(corpus:do-lines (line ,filename)
     (let ((,sentence (line->sentence line ,vocabulary)))
       ,@body)))


(defun build-vocabulary (pathnames)
  (let ((vocab (voc:make-vocabulary))
        (word-key->freq (make-hash-table :test #'equal)))
    (dolist (pathname pathnames)
      (corpus:do-lines (line pathname)
        (dolist (word (line-units line))
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
        (corpus:do-lines (line pathname)
          (dolist (word (line-units line))
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
                 (do-sentences (sentence pathname vocabulary)
                   (add-counts (ngram:sentence-tokens sentence)))))
             freq))
         (build-corpus (p vocabulary)
           (let ((sentence-list nil))
             (do-sentences (s p vocabulary)
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
    (corpus:do-lines (line pathname)
      (dolist (unit (line-units line))
        (when (dict:contains-p trusted-word-dictionary unit)
          (voc:add-new vocabulary (unit:unit->key unit))))))
  vocabulary)

(defun build-word-dictionary (pathnames vocabulary)
  (format *error-output* "Building dictionary ...~%")
  (let ((dict (dict:make-dictionary)))
    (dolist (pathname pathnames)
      (corpus:do-lines (line pathname)
        (dolist (unit (line-units line))
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
      (do-sentences (sentence pathname vocabulary)
        (model-add-counts sentence))))
  model)

(defun build-unknown-word-vocabulary (pathnames vocabulary &key (overlap 2))
  (format *error-output* "Building unknown word vocabulary ...~%")
  (let ((key->freq (make-hash-table :test #'equal))
        (pron-vocab (voc:make-vocabulary)))
    (dolist (pathname pathnames)
      (let ((curr-prons (make-hash-table :test #'equal)))
        (corpus:do-lines (line pathname)
          (dolist (unit (line-units line))
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
        (corpus:do-lines (line pathname)
          (dolist (unit (line-units line))
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

;;;

(defun build-kkc-simple (pathnames
                         &key class-token-to-word-file-path
                              (weights (list 0.8d0 0.2d0))
                              char-dictionary)
  (let ((vocabulary (build-vocabulary pathnames)))
    (let ((ngram-model
           (if class-token-to-word-file-path
               (make-instance 'hachee.language-model.ngram:class-model
                              :classifier
                              (build-classifier class-token-to-word-file-path
                                                vocabulary))
               (make-instance 'hachee.language-model.ngram:model
                              :weights weights))))
      (train-ngram-model ngram-model pathnames vocabulary)
      (make-instance (ecase (length weights)
                       (2 'hachee.kkc.impl.lm:kkc-2gram)
                       (3 'hachee.kkc.impl.lm:kkc-3gram))
       :ngram-model ngram-model
       :vocabulary vocabulary
       :word-dictionary
       (build-word-dictionary pathnames vocabulary)
       :char-dictionary (or char-dictionary
                            (hachee.kkc.impl.lm.dictionary:make-dictionary))
       :unknown-word-vocabulary
       (hachee.language-model.vocabulary:make-vocabulary)
       :unknown-word-ngram-model
       (make-instance 'hachee.language-model.ngram:model)))))

(defun build-kkc (pathnames-segmented
                  &key pathnames-inaccurately-segmented
                       word-dictionary-pathnames
                       char-dictionary
                       trusted-word-dictionary
                       class-token-to-word-file-path
                       (weights (list 0.8d0 0.2d0)))
  (let ((vocabulary (build-vocabulary-with-unk pathnames-segmented)))
    (when (and pathnames-inaccurately-segmented
               trusted-word-dictionary
               ;; Unable to map an added word to a class
               (not class-token-to-word-file-path))
      (extend-existing-vocabulary vocabulary
                                  trusted-word-dictionary
                                  pathnames-inaccurately-segmented))
    (let ((pathnames
           (append pathnames-segmented
                   pathnames-inaccurately-segmented))
          (ngram-model
           (if class-token-to-word-file-path
               (make-instance 'hachee.language-model.ngram:class-model
                              :classifier
                              (build-classifier class-token-to-word-file-path
                                                vocabulary)
                              :weights weights)
               (make-instance 'hachee.language-model.ngram:model
                              :weights weights))))
      (train-ngram-model ngram-model pathnames vocabulary)
      (let* ((word-dictionary
              (add-to-word-dictionary-from-resources
               (build-word-dictionary pathnames vocabulary)
               word-dictionary-pathnames))
             (unknown-word-vocabulary
              (build-unknown-word-vocabulary pathnames vocabulary))
             (unknown-word-ngram-model
              (build-unknown-word-ngram-model pathnames
                                              vocabulary
                                              unknown-word-vocabulary)))
        (make-instance (ecase (length weights)
                         (2 'hachee.kkc.impl.lm:kkc-2gram)
                         (3 'hachee.kkc.impl.lm:kkc-3gram))
         :ngram-model ngram-model
         :vocabulary vocabulary
         :word-dictionary word-dictionary
         :char-dictionary
         (or char-dictionary
             (hachee.kkc.impl.lm.dictionary:make-dictionary))
         :unknown-word-vocabulary unknown-word-vocabulary
         :unknown-word-ngram-model unknown-word-ngram-model)))))
