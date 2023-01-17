(defpackage :senn-kkc-engine.hachee.engine.class-2-gram
  (:use :cl)
  (:export :set-kkc
           :main))
(in-package :senn-kkc-engine.hachee.engine.class-2-gram)

;;; TODO: user-dict

;;;

(defvar *kkc*)

(defun set-kkc (&optional dir-pathname)
  (unless dir-pathname
    (setq dir-pathname
          (merge-pathnames ".senn/class-2-gram/" (user-homedir-pathname))))
  (setq *kkc* (hachee.kkc.impl.class-2-gram:read-kkc
               (merge-pathnames "class-vocab.txt" dir-pathname)
               (merge-pathnames "class-weights.txt" dir-pathname)
               (merge-pathnames "class-ngram-counts.txt" dir-pathname)
               (merge-pathnames "class-word-counts.txt" dir-pathname)
               (merge-pathnames "unk-vocab.txt" dir-pathname)
               (merge-pathnames "unk-ngram-counts.txt" dir-pathname)
               (merge-pathnames "char-set-size.txt" dir-pathname)))
  (values))

(defun main ()
  (labels ((handle (line)
             (senn-kkc-engine.hachee.engine:handle line *kkc*)))
    (senn-ipc.server.stdio:start-server #'handle)))
