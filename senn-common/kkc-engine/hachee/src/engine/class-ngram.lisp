(defpackage :senn-kkc-engine.hachee.engine.class-ngram
  (:use :cl)
  (:export :set-kkc-2gram
           :set-kkc-3gram
           :main))
(in-package :senn-kkc-engine.hachee.engine.class-ngram)

;;; user-dict

(defstruct user-dict entries)
(defstruct user-dict-entry pron form)

(defun read-user-dict ()
  (let ((path (merge-pathnames ".senn/user-dict.txt"
                               (user-homedir-pathname))))
    (when (uiop/filesystem:file-exists-p path)
      (let ((entries nil))
        (with-open-file (in path
                            :direction :input
                            :external-format :utf-8)
          (loop for line = (read-line in nil nil) while line do
            (when (and (not (string= line ""))
                       (not (char= (char line 0) #\#)))
              (destructuring-bind (form pron)
                  (cl-ppcre:split "\\s" line)
                (when (and (not (string= form ""))
                           (not (string= pron "")))
                  (push (make-user-dict-entry :pron pron
                                              :form form)
                        entries))))))
        (make-user-dict :entries (nreverse entries))))))

(defmethod hachee.kkc.impl.class-ngram.ex-dict-builder:item-pron
    ((item user-dict-entry))
  (user-dict-entry-pron item))

(defmethod hachee.kkc.impl.class-ngram.ex-dict-builder:item-form
    ((item user-dict-entry))
  (user-dict-entry-form item))

(defmethod hachee.kkc.impl.class-ngram.ex-dict-builder:list-items
    ((source user-dict))
  (user-dict-entries source))

;;;

(defvar *kkc*)

(defun start-server ()
  (let ((dict (read-user-dict)))
    (when dict
      (hachee.kkc.impl.class-ngram:set-ex-dict *kkc* dict)))
  (labels ((handle (line)
             (senn-kkc-engine.hachee.engine:handle line *kkc*)))
    (senn-ipc.server.stdio:start-server #'handle)))

(defun show-user-dict ()
  (let ((dict (read-user-dict)))
    (when dict
      (format *standard-output* "form pron~%")
      (dolist (entry (user-dict-entries dict))
        (format *standard-output* "~A ~A~%"
                (user-dict-entry-form entry)
                (user-dict-entry-pron entry)))
      (force-output *standard-output*)))
  (values))

(defun set-kkc-2gram (dir-pathname)
  (setq *kkc* (hachee.kkc.impl.class-ngram:read-kkc-2gram
               (merge-pathnames "class-weights.txt" dir-pathname)
               (merge-pathnames "class-vocab.txt" dir-pathname)
               (merge-pathnames "class-ngram-counts.txt" dir-pathname)
               (merge-pathnames "class-word-counts.txt" dir-pathname)
               (merge-pathnames "unk-vocab.txt" dir-pathname)
               (merge-pathnames "unk-ngram-counts.txt" dir-pathname)
               (merge-pathnames "char-set-size.txt" dir-pathname)))
  (values))

(defun set-kkc-3gram (dir-pathname)
  (setq *kkc* (hachee.kkc.impl.class-ngram:read-kkc-3gram
               (merge-pathnames "class-weights.txt" dir-pathname)
               (merge-pathnames "class-vocab.txt" dir-pathname)
               (merge-pathnames "class-ngram-counts.txt" dir-pathname)
               (merge-pathnames "class-word-counts.txt" dir-pathname)
               (merge-pathnames "unk-vocab.txt" dir-pathname)
               (merge-pathnames "unk-ngram-counts.txt" dir-pathname)
               (merge-pathnames "char-set-size.txt" dir-pathname)))
  (values))

(defun main ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (cond ((null args)
           (start-server))
          ((string= (car args) "--show-userdict")
           (show-user-dict)))))
