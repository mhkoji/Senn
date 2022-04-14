(defpackage :senn-user-dict
  (:use :cl)
  (:export :dict
           :entry-form
           :entry-pron
           :entry
           :dict-entries
           :read-file))
(in-package :senn-user-dict)

(defun user-dict-pathname ()
  (merge-pathnames ".senn/user-dict.txt"
                   (user-homedir-pathname)))

;;;

(defstruct entry form pron)

(defstruct dict entries)

(defun read-file-internal (path)
  (let ((ptr (cffi:with-foreign-string (foreign-path (namestring path))
               (senn-user-dict.cffi:user-dict-load foreign-path))))
    (when (not (cffi:null-pointer-p ptr))
      (unwind-protect
           (let ((count (senn-user-dict.cffi:user-dict-count ptr))
                 (entries nil))
             (loop for i from 0 below count
                   for entry = (senn-user-dict.cffi:user-dict-entry ptr i)
                   for form = (senn-user-dict.cffi:entry-form entry)
                   for pron = (senn-user-dict.cffi:entry-pron entry)
                   do (progn
                        (push (make-entry :form form :pron pron)
                              entries)))
             (make-dict :entries (nreverse entries)))
        (senn-user-dict.cffi:user-dict-destroy ptr)))))

(defun read-file ()
  (let ((path (user-dict-pathname)))
    (when (uiop/filesystem:file-exists-p path)
      (read-file-internal path))))
