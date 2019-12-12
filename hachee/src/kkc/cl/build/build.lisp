(defpackage :hachee.kkc.build
  (:use :cl)
  (:import-from :hachee.language-model.vocabulary
                :add-new
                :to-int
                :to-int-or-nil
                :to-int-or-unk)
  (:import-from :hachee.kkc.word
                :make-char
                :char->key
                :make-word
                :word-form
                :word-pron
                :word->pron-chars
                :word->key)
  (:export :build-vocabulary
           :build-vocabulary-with-unk
           :extend-existing-vocabulary
           :build-dictionary
           :build-n-gram-model
           :build-unknown-word-vocabulary
           :build-unknown-word-n-gram-model
           :build-word-dictionary
           :build-tankan-dictionary))
(in-package :hachee.kkc.build)

(defun to-token-sentence (file-sentence vocabulary)
  (hachee.language-model:make-sentence
   :tokens (mapcar (lambda (w)
                     (to-int-or-unk vocabulary (word->key w)))
                   (hachee.kkc.build.file:sentence-words file-sentence))))

(defun build-vocabulary (pathnames)
  (let ((vocab (hachee.language-model.vocabulary:make-vocabulary)))
    (dolist (pathname pathnames)
      (dolist (sentence (hachee.kkc.build.file:file->sentences pathname))
        (dolist (word (hachee.kkc.build.file:sentence-words sentence))
          (add-new vocab (word->key word)))))
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
      (dolist (word (hachee.kkc.build.file:sentence-words sentence))
        (when (hachee.kkc.word.dictionary:contains-word-p
               trusted-word-dictionary
               word)
          (add-new vocabulary word)))))
    vocabulary)

(defun build-dictionary (pathnames vocabulary)
  (log:info "Building dictionary...")
  (let ((dict (hachee.kkc.word.dictionary:make-dictionary)))
    (dolist (pathname pathnames dict)
      (dolist (sentence (hachee.kkc.build.file:file->sentences pathname))
        (dolist (word (hachee.kkc.build.file:sentence-words sentence))
          (when (to-int-or-nil vocabulary (word->key word))
            (hachee.kkc.word.dictionary:add-word dict word)))))
    dict))

(defun build-n-gram-model (pathnames vocabulary)
  (log:info "Building n-gram model...")
  (let ((BOS (to-int vocabulary hachee.language-model.vocabulary:+BOS+))
        (EOS (to-int vocabulary hachee.language-model.vocabulary:+EOS+))
        (model (make-instance 'hachee.language-model.n-gram:model)))
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
      (let ((curr-chars (make-hash-table :test #'equal)))
        (dolist (sentence (hachee.kkc.build.file:file->sentences pathname))
          (dolist (word (hachee.kkc.build.file:sentence-words sentence))
            (when (not (to-int-or-nil vocabulary (word->key word)))
              (dolist (char (word->pron-chars word))
                (setf (gethash (char->key char) curr-chars) char)))))
        (maphash (lambda (key char)
                   (let ((freq (incf (gethash key key->freq 0))))
                     (when (<= overlap freq)
                       (add-new char-vocab (char->key char)))))
                 curr-chars)))
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
      (dolist (word (hachee.kkc.build.file:sentence-words file-sentence))
        (when (not (to-int-or-nil vocabulary (word->key word)))
          (let ((sentence (hachee.kkc.util:word->sentence
                           word
                           unknown-word-vocabulary)))
            (hachee.language-model.n-gram:train model (list sentence)
                                                :BOS BOS
                                                :EOS EOS))))))
  model))

(defun build-word-dictionary (pathnames)
  (log:info "Building...")
  (let ((dict (hachee.kkc.word.dictionary:make-dictionary)))
    (dolist (pathname pathnames)
      (with-open-file (in pathname)
        (loop for line = (read-line in nil nil) while line do
          (destructuring-bind (form part pron) (cl-ppcre:split "/" line)
            (declare (ignore part))
            (let ((word (make-word :form form :pron pron)))
              (hachee.kkc.word.dictionary:add-word dict word))))))
    dict))

(defun build-tankan-dictionary (pathnames)
  (let ((dict (hachee.kkc.word.dictionary:make-dictionary)))
    (dolist (pathname pathnames)
      (with-open-file (in pathname)
        (loop for line = (read-line in nil nil) while line do
          (destructuring-bind (form pron) (cl-ppcre:split "/" line)
            (let ((char (make-char :form form :pron pron)))
              (hachee.kkc.word.dictionary:add-char dict char))))))
    dict))
