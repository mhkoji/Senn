(defpackage :senn.prefix-dictionary
  (:use :cl)
  (:export :lookup
           :dictionary
           :save-dictionary
           :load-dictionary
           :make-dictionary
           :build-dictionary
           :lookup))
(in-package :senn.prefix-dictionary)

(defstruct dictionary
  (trie (make-instance 'cl-trie:trie)))

(defun save-dictionary (dict stream)
  (let ((trie (dictionary-trie dict)))
    (print (list :key-value-alist
                 (mapcar (lambda (k)
                           (cons k (cl-trie:lookup trie k)))
                         (cl-trie:all-keys trie)))
           stream))
  (values))

(defun load-dictionary (stream)
  (let ((trie (make-instance 'cl-trie:trie)))
    (let ((list (read stream)))
      (loop for (key . value) in (getf list :key-value-alist)
            do (progn (setf (cl-trie:lookup trie key) value))))
    (make-dictionary :trie trie)))

(defun build-dictionary (pathnames)
  (log:info "Building...")
  (let ((trie (make-instance 'cl-trie:trie)))
    (dolist (pathname pathnames)
      (dolist (sentence (hachee.kkc.build.file:file->sentences pathname))
        (dolist (unit (hachee.kkc.build.file:sentence-units sentence))
          (let ((pron (hachee.kkc.dictionary:unit-pron unit)))
            (loop for i from 1 below (length pron)
                  for prefix = (subseq pron 0 i)
                  do (pushnew unit (cl-trie:lookup trie prefix)
                              :test #'hachee.kkc.dictionary:unit=))))))
    (make-dictionary :trie trie)))

(defun lookup (dictionary prefix)
  (cl-trie:lookup (dictionary-trie dictionary) prefix))
