(defpackage :hachee.kkc
  (:use :cl)
  (:export :convert
           :convert-to-nodes

           :lookup-forms
           :lookup-items

           :profile
           :make-kkc
           :create-simple-kkc
           :create-kkc
           :save-kkc
           :load-kkc

           :word-form
           :word-pron

           :build-word-dictionary
           :build-tankan-dictionary)
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
  (:import-from :alexandria
                :curry))
(in-package :hachee.kkc)

(defun to-token-sentence (file-sentence vocabulary)
  (hachee.language-model:make-sentence
   :tokens (mapcar (lambda (w)
                     (to-int-or-unk vocabulary (word->key w)))
                   (hachee.kkc.file:sentence-words file-sentence))))

(defun build-vocabulary (pathnames)
  (let ((vocab (hachee.language-model.vocabulary:make-vocabulary)))
    (dolist (pathname pathnames)
      (dolist (sentence (hachee.kkc.file:file->sentences pathname))
        (dolist (word (hachee.kkc.file:sentence-words sentence))
          (add-new vocab (word->key word)))))
    vocab))

(defun build-vocabulary-with-unk (pathnames &key (overlap 2))
  (let ((vocab (hachee.language-model.vocabulary:make-vocabulary))
        (word-key->freq (make-hash-table :test #'equal)))
    (dolist (pathname pathnames)
      (let ((curr-words (make-hash-table :test #'equal)))
        (dolist (sentence (hachee.kkc.file:file->sentences pathname))
          (dolist (word (hachee.kkc.file:sentence-words sentence))
            (setf (gethash (word->key word) curr-words) word)))
        (maphash (lambda (word-key word)
                   (let ((freq (incf (gethash word-key word-key->freq 0))))
                     (when (<= overlap freq)
                       (add-new vocab (word->key word)))))
                 curr-words)))
    vocab))

(defun build-dictionary (pathnames vocabulary)
  (let ((dict (hachee.kkc.word.dictionary:make-dictionary)))
    (dolist (pathname pathnames dict)
      (dolist (sentence (hachee.kkc.file:file->sentences pathname))
        (dolist (word (hachee.kkc.file:sentence-words sentence))
          (when (to-int-or-nil vocabulary (word->key word))
            (hachee.kkc.word.dictionary:add-word dict word)))))
    dict))

(defun build-n-gram-model (pathnames vocabulary)
  (let ((BOS (to-int vocabulary hachee.language-model.vocabulary:+BOS+))
        (EOS (to-int vocabulary hachee.language-model.vocabulary:+EOS+))
        (model (make-instance 'hachee.language-model.n-gram:model)))
    (dolist (pathname pathnames)
      (let ((sentences (mapcar (lambda (s)
                                 (to-token-sentence s vocabulary))
                               (hachee.kkc.file:file->sentences pathname))))
        (hachee.language-model.n-gram:train model sentences
                                            :BOS BOS
                                            :EOS EOS)))
    model))

(defun build-unknown-word-vocabulary (pathnames vocabulary &key (overlap 2))
  (let ((key->freq (make-hash-table :test #'equal))
        (char-vocab (hachee.language-model.vocabulary:make-vocabulary)))
    (dolist (pathname pathnames)
      (let ((curr-chars (make-hash-table :test #'equal)))
        (dolist (sentence (hachee.kkc.file:file->sentences pathname))
          (dolist (word (hachee.kkc.file:sentence-words sentence))
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
  (let ((BOS (to-int unknown-word-vocabulary
                     hachee.language-model.vocabulary:+BOS+))
        (EOS (to-int unknown-word-vocabulary
                     hachee.language-model.vocabulary:+EOS+))
        (model (make-instance 'hachee.language-model.n-gram:model)))
  (dolist (pathname pathnames)
    (dolist (file-sentence (hachee.kkc.file:file->sentences pathname))
      (dolist (word (hachee.kkc.file:sentence-words file-sentence))
        (when (not (to-int-or-nil vocabulary (word->key word)))
          (let ((sentence (hachee.kkc.util:word->sentence
                           word
                           unknown-word-vocabulary)))
            (hachee.language-model.n-gram:train model (list sentence)
                                                :BOS BOS
                                                :EOS EOS))))))
  model))

(defun build-word-dictionary (pathnames)
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

(defstruct simple-kkc
  n-gram-model
  vocabulary
  vocabulary-dictionary
  extended-dictionary
  word-dictionary
  tankan-dictionary)

(defstruct (kkc (:include simple-kkc))
  sum-probabilities-of-vocabulary-words
  unknown-word-vocabulary
  unknown-word-n-gram-model)

(defun create-simple-kkc (pathnames &key word-dictionary tankan-dictionary)
  (let* ((vocabulary (build-vocabulary pathnames))
         (dictionary (build-dictionary pathnames vocabulary))
         (n-gram-model (build-n-gram-model pathnames vocabulary)))
    (make-simple-kkc
     :n-gram-model n-gram-model
     :vocabulary vocabulary
     :vocabulary-dictionary dictionary
     :extended-dictionary (hachee.kkc.word.dictionary:make-dictionary)
     :word-dictionary (or word-dictionary
                          (hachee.kkc.word.dictionary:make-dictionary))
     :tankan-dictionary (or tankan-dictionary
                            (hachee.kkc.word.dictionary:make-dictionary)))))

(defun sum-probabilities-of-words (unknown-word-vocabulary
                                   unknown-word-n-gram-model
                                   words)
  (let ((bos-token (to-int unknown-word-vocabulary
                           hachee.language-model.vocabulary:+BOS+))
        (eos-token (to-int unknown-word-vocabulary
                           hachee.language-model.vocabulary:+EOS+)))
    (loop
      for word in words
      sum (exp (hachee.language-model.n-gram:sentence-log-probability
                unknown-word-n-gram-model
                (hachee.kkc.util:word->sentence
                 word
                 unknown-word-vocabulary)
                :BOS bos-token
                :EOS eos-token)))))

(defun create-kkc (pathnames &key word-dictionary
                                  tankan-dictionary
                                  extended-dictionary)
  (assert (<= 2 (length pathnames)))
  (let* ((vocabulary (build-vocabulary-with-unk pathnames))
         (dictionary (build-dictionary pathnames vocabulary))
         (n-gram-model (build-n-gram-model pathnames vocabulary))
         (unknown-word-vocabulary (build-unknown-word-vocabulary
                                   pathnames
                                   vocabulary))
         (unknown-word-n-gram-model (build-unknown-word-n-gram-model
                                     pathnames
                                     vocabulary
                                     unknown-word-vocabulary)))
    (make-kkc
     :vocabulary vocabulary
     :n-gram-model n-gram-model
     :vocabulary-dictionary dictionary
     :extended-dictionary (or extended-dictionary
                              (hachee.kkc.word.dictionary:make-dictionary))
     :word-dictionary (or word-dictionary
                          (hachee.kkc.word.dictionary:make-dictionary))
     :tankan-dictionary (or tankan-dictionary
                            (hachee.kkc.word.dictionary:make-dictionary))
     :unknown-word-vocabulary unknown-word-vocabulary
     :unknown-word-n-gram-model unknown-word-n-gram-model
     :sum-probabilities-of-vocabulary-words
     (sum-probabilities-of-words
      unknown-word-vocabulary
      unknown-word-n-gram-model
      (hachee.kkc.word.dictionary:list-all dictionary)))))

(defgeneric get-score-fn (abstract-kkc))

(defmethod get-score-fn ((kkc simple-kkc))
  (hachee.kkc.convert.score-fns:of-form-pron-simple
   :word-vocabulary (simple-kkc-vocabulary kkc)
   :word-n-gram-model (simple-kkc-n-gram-model kkc)))

(defmethod get-score-fn ((kkc kkc))
  (hachee.kkc.convert.score-fns:of-form-pron
   :word-vocabulary (kkc-vocabulary kkc)
   :word-n-gram-model (kkc-n-gram-model kkc)
   :unknown-word-char-vocabulary (kkc-unknown-word-vocabulary kkc)
   :unknown-word-char-n-gram-model (kkc-unknown-word-n-gram-model kkc)
   :probability-for-extended-dictionary-words
   (let ((extended-dictionary-size
          (hachee.kkc.word.dictionary:size (kkc-extended-dictionary kkc)))
         (sum-probabilities-of-vocabulary-words
          (kkc-sum-probabilities-of-vocabulary-words kkc)))
     (if (< 0 extended-dictionary-size)
         (/ sum-probabilities-of-vocabulary-words
            extended-dictionary-size)
         0))))

(defun convert-to-nodes (abstract-kkc pronunciation &key 1st-boundary-index)
  (hachee.kkc.convert:execute pronunciation
   :score-fn (get-score-fn abstract-kkc)
   :vocabulary (simple-kkc-vocabulary abstract-kkc)
   :vocabulary-dictionary (simple-kkc-vocabulary-dictionary abstract-kkc)
   :extended-dictionary (simple-kkc-extended-dictionary abstract-kkc)
   :1st-boundary-index 1st-boundary-index))

(defun convert (abstract-kkc pronunciation &key 1st-boundary-index)
  (mapcar #'hachee.kkc.convert:node-word
          (convert-to-nodes abstract-kkc pronunciation
                            :1st-boundary-index 1st-boundary-index)))

(defun lookup-items (abstract-kkc pronunciation)
  (hachee.kkc.lookup:execute pronunciation
   :word-dicts
   (list (list :vocabulary
               (simple-kkc-vocabulary-dictionary abstract-kkc)
               :extended-dictionary
               (simple-kkc-extended-dictionary abstract-kkc)
               :word-dictionary
               (simple-kkc-word-dictionary abstract-kkc)))
   :char-dicts
   (list (list :tankan-dictionary
               (simple-kkc-tankan-dictionary abstract-kkc)))))

(defun lookup-forms (abstract-kkc pronunciation)
  (mapcar #'hachee.kkc.lookup:item-form
          (lookup-items abstract-kkc pronunciation)))


(defun save-kkc (kkc pathname)
  (hachee.kkc.archive:save
   pathname
   :n-gram-model (kkc-n-gram-model kkc)
   :vocabulary (kkc-vocabulary kkc)
   :vocabulary-dictionary (kkc-vocabulary-dictionary kkc)
   :extended-dictionary (kkc-extended-dictionary kkc)
   :word-dictionary (kkc-word-dictionary kkc)
   :tankan-dictionary (kkc-tankan-dictionary kkc)
   :unknown-word-vocabulary (kkc-unknown-word-vocabulary kkc)
   :unknown-word-n-gram-model (kkc-unknown-word-n-gram-model kkc)))


(defun load-kkc (pathname)
  (destructuring-bind (&key n-gram-model
                            vocabulary
                            vocabulary-dictionary
                            extended-dictionary
                            word-dictionary
                            tankan-dictionary
                            unknown-word-vocabulary
                            unknown-word-n-gram-model)
      (hachee.kkc.archive:load pathname)
    (make-kkc
     :n-gram-model n-gram-model
     :vocabulary vocabulary
     :vocabulary-dictionary vocabulary-dictionary
     :extended-dictionary extended-dictionary
     :tankan-dictionary tankan-dictionary
     :word-dictionary word-dictionary
     :unknown-word-vocabulary unknown-word-vocabulary
     :unknown-word-n-gram-model unknown-word-n-gram-model
     :sum-probabilities-of-vocabulary-words
     (sum-probabilities-of-words
      unknown-word-vocabulary
      unknown-word-n-gram-model
      (hachee.kkc.word.dictionary:list-all vocabulary-dictionary)))))
