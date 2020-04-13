(defpackage :hachee.kkc.build
  (:use :cl)
  (:import-from :hachee.language-model.vocabulary
                :add-new
                :to-int
                :to-int-or-nil
                :to-int-or-unk)
  (:import-from :hachee.kkc.dictionary
                :make-unit
                :unit-form
                :unit-pron
                :unit->key
                :unit->pron-chars)
  (:export :build-vocabulary
           :build-vocabulary-with-unk
           :extend-existing-vocabulary
           :add-dictionary-entries-from-files
           :train-n-gram-model
           :build-unknown-word-vocabulary
           :build-unknown-word-n-gram-model
           :build-word-dictionary
           :build-tankan-dictionary
           :build-classifier))
(in-package :hachee.kkc.build)

(defun to-token-sentence (file-sentence vocabulary)
  (hachee.language-model:make-sentence
   :tokens
   (mapcar (lambda (u)
             (to-int-or-unk vocabulary (unit->key u)))
           (hachee.kkc.build.file:sentence-units file-sentence))))

(defun build-vocabulary (pathnames)
  (let ((vocab (hachee.language-model.vocabulary:make-vocabulary)))
    (dolist (pathname pathnames)
      (dolist (sentence (hachee.kkc.build.file:file->sentences pathname))
        (dolist (word (hachee.kkc.build.file:sentence-units sentence))
          (add-new vocab (unit->key word)))))
    vocab))

(defun build-vocabulary-with-unk (pathnames &key (overlap 2))
  (assert (<= overlap (length pathnames)))
  (log:info "Building initial vocabulary...")
  (let ((vocab (hachee.language-model.vocabulary:make-vocabulary))
        (word-key->freq (make-hash-table :test #'equal)))
    (dolist (pathname pathnames)
      (let ((curr-words (make-hash-table :test #'equal)))
        (dolist (sentence (hachee.kkc.build.file:file->sentences pathname))
          (dolist (word (hachee.kkc.build.file:sentence-words sentence))
            (setf (gethash (word->key word) curr-words) word)))
        (maphash (lambda (word-key word)
                   (let ((freq (incf (gethash word-key word-key->freq 0))))
                     (when (<= overlap freq)
                       (add-new vocab (word->key word)))))
                 curr-words)))
    vocab))

(defun extend-existing-vocabulary (vocabulary
                                   trusted-word-dictionary
                                   pathnames-inaccurately-segmented)
  (log:info "Extending vocabulary...")
  (dolist (pathname pathnames-inaccurately-segmented)
    (dolist (sentence (hachee.kkc.build.file:file->sentences pathname))
      (dolist (unit (hachee.kkc.build.file:sentence-units sentence))
        (when (hachee.kkc.dictionary:contains-p trusted-word-dictionary unit)
          (add-new vocabulary (unit->key unit))))))
  vocabulary)

(defun add-dictionary-entries-from-files (dict pathnames vocabulary)
  (log:info "Building dictionary...")
  (dolist (pathname pathnames)
    (dolist (sentence (hachee.kkc.build.file:file->sentences pathname))
      (dolist (unit (hachee.kkc.build.file:sentence-units sentence))
        (if (to-int-or-nil vocabulary (unit->key unit))
            (hachee.kkc.dictionary:add-entry
             dict word hachee.kkc.dictionary:+origin-vocabulary+)
            (hachee.kkc.dictionary:add-entry
             dict word hachee.kkc.dictionary:+origin-corpus+)))))
  dict)

(defun train-n-gram-model (model pathnames vocabulary)
  (log:info "Building n-gram model...")
  (let ((BOS (to-int vocabulary hachee.language-model.vocabulary:+BOS+))
        (EOS (to-int vocabulary hachee.language-model.vocabulary:+EOS+)))
    (dolist (pathname pathnames)
      (let ((sentences
             (mapcar (lambda (s)
                       (to-token-sentence s vocabulary))
                     (hachee.kkc.build.file:file->sentences pathname))))
        (hachee.language-model.n-gram:train model sentences
                                            :BOS BOS
                                            :EOS EOS)))
    model))

(defun build-unknown-word-vocabulary (pathnames vocabulary &key (overlap 2))
  (log:info "Building...")
  (let ((key->freq (make-hash-table :test #'equal))
        (char-vocab (hachee.language-model.vocabulary:make-vocabulary)))
    (dolist (pathname pathnames)
      (let ((curr-prons (make-hash-table :test #'equal)))
        (dolist (sentence (hachee.kkc.build.file:file->sentences pathname))
          (dolist (unit (hachee.kkc.build.file:sentence-units sentence))
            (when (not (to-int-or-nil vocabulary (unit->key unit)))
              (dolist (pron-unit (unit->pron-units unit))
                (setf (gethash (unit->key pron-unit) curr-prons)
                      pron-unit)))))
        (maphash (lambda (key pron-unit)
                   (let ((freq (incf (gethash key key->freq 0))))
                     (when (<= overlap freq)
                       (add-new char-vocab (unit->key pron-unit)))))
                 curr-prons)))
    char-vocab))

(defun build-unknown-word-n-gram-model (pathnames
                                        vocabulary
                                        unknown-word-vocabulary)
  (log:info "Building...")
  (let ((BOS (to-int unknown-word-vocabulary
                     hachee.language-model.vocabulary:+BOS+))
        (EOS (to-int unknown-word-vocabulary
                     hachee.language-model.vocabulary:+EOS+))
        (model (make-instance 'hachee.language-model.n-gram:model)))
  (dolist (pathname pathnames)
    (dolist (file-sentence (hachee.kkc.build.file:file->sentences pathname))
      (dolist (unit (hachee.kkc.build.file:sentence-units file-sentence))
        (when (not (to-int-or-nil vocabulary (unit->key unit)))
          (let ((sentence (hachee.kkc.util:unit->sentence
                           unit
                           unknown-word-vocabulary)))
            (hachee.language-model.n-gram:train model (list sentence)
                                                :BOS BOS
                                                :EOS EOS))))))
  model))

(defun build-word-dictionary (dict pathnames)
  (log:info "Building...")
  (dolist (pathname pathnames)
    (with-open-file (in pathname)
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (form part pron) (cl-ppcre:split "/" line)
          (declare (ignore part))
          (let ((unit (make-unit :form form :pron pron)))
            (hachee.kkc.dictionary:add-entry
             dict word +origin-resource+))))))
  dict)

(defun build-tankan-dictionary (dict pathnames)
  (dolist (pathname pathnames)
    (with-open-file (in pathname)
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (form pron) (cl-ppcre:split "/" line)
          (let ((char-unit (make-unit :form form
                                      :pron pron)))
            (hachee.kkc.dictionary:add-entry dict char-unit :tankan))))))
  dict)

(defun build-classifier (class-token-to-word-file-path vocabulary)
  (let ((to-class-map (make-hash-table :test #'equal)))
    (setf (gethash (to-int vocabulary hachee.language-model.vocabulary:+UNK+)
                   to-class-map)
          0)
    (setf (gethash (to-int vocabulary hachee.language-model.vocabulary:+BOS+)
                   to-class-map)
          -1)
    (setf (gethash (to-int vocabulary hachee.language-model.vocabulary:+EOS+)
                   to-class-map)
          -2)
    (with-open-file (in class-token-to-word-file-path)
      (read-line in nil nil) ;; UT
      (read-line in nil nil) ;; BT
      (loop for line = (read-line in nil nil) while line do
        (destructuring-bind (class-token-string form-pron-string)
            (cl-ppcre:split " " line)
          (let ((class-token (parse-integer class-token-string)))
            (let* ((forms
                    (mapcar (lambda (x) (first (cl-ppcre:split "/" x)))
                            (cl-ppcre:split "-" form-pron-string)))
                   (prons
                    (mapcar (lambda (x) (second (cl-ppcre:split "/" x)))
                            (cl-ppcre:split "-" form-pron-string)))
                   (token
                    (to-int vocabulary
                            (unit->key
                             (make-unit
                              :form (apply #'concatenate 'string forms)
                              :pron (apply #'concatenate 'string prons))))))
              (setf (gethash token to-class-map) class-token))))))
    (hachee.language-model.n-gram:make-classifier
     :to-class-map to-class-map)))
