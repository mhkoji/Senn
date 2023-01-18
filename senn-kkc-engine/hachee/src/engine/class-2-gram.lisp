(defpackage :senn-kkc-engine.hachee.engine.class-2-gram
  (:use :cl)
  (:export :set-kkc
           :main)
  (:local-nicknames (:user-dict :senn-kkc-engine.hachee.user-dict)))
(in-package :senn-kkc-engine.hachee.engine.class-2-gram)

;;; user-dict

(defmethod hachee.kkc.impl.class-2-gram.ex-dict-builder:item-pron
    ((item user-dict:entry))
  (user-dict:entry-pron item))

(defmethod hachee.kkc.impl.class-2-gram.ex-dict-builder:item-form
    ((item user-dict:entry))
  (user-dict:entry-form item))

(defmethod hachee.kkc.impl.class-2-gram.ex-dict-builder:list-items
    ((source user-dict:dict))
  (user-dict:dict-entries source))

(defun kkc-apply-user-dict (kkc)
  (let ((dict (user-dict:read-file)))
    (when dict
      (hachee.kkc.impl.class-2-gram:set-ex-dict kkc dict))))

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
  (handler-case
      (let ((dirs (list
                   "/usr/lib/senn/fcitx/kkc/"    ;; for fcitx
                   "/usr/lib/senn/ibus/kkc/")))  ;; for ibus
        (user-dict:with-library-loaded (dirs)
          (kkc-apply-user-dict *kkc*)))
    (error (e)
      (format *standard-output* "~A~%" e)))
  (labels ((handle (line)
             (senn-kkc-engine.hachee.engine:handle line *kkc*)))
    (senn-ipc.server.stdio:start-server #'handle)))
