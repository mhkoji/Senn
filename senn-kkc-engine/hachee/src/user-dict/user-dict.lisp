(defpackage :senn-kkc-engine.hachee.user-dict
  (:use :cl)
  (:export :dict
           :entry-form
           :entry-pron
           :entry
           :dict-entries
           :read-file)
  (:local-nicknames (:user-dict.cffi :senn-kkc-engine.hachee.user-dict.cffi)))
(in-package :senn-kkc-engine.hachee.user-dict.cffi)

(defun user-dict-pathname ()
  (merge-pathnames ".senn/user-dict.txt"
                   (user-homedir-pathname)))

;;;

(defstruct entry form pron)

(defstruct dict entries)

(defun read-file-internal (path)
  (let ((ptr (cffi:with-foreign-string (foreign-path (namestring path))
               (user-dict.cffi:user-dict-load foreign-path))))
    (when (not (cffi:null-pointer-p ptr))
      (unwind-protect
           (let ((count (user-dict.cffi:user-dict-count ptr))
                 (entries nil))
             (loop for i from 0 below count
                   for entry = (user-dict.cffi:user-dict-entry ptr i)
                   for form = (user-dict.cffi:entry-form entry)
                   for pron = (user-dict.cffi:entry-pron entry)
                   do (progn
                        (push (make-entry :form form :pron pron)
                              entries)))
             (make-dict :entries (nreverse entries)))
        (user-dict.cffi:user-dict-destroy ptr)))))

(defun read-file ()
  (let ((path (user-dict-pathname)))
    (when (uiop/filesystem:file-exists-p path)
      (read-file-internal path))))
