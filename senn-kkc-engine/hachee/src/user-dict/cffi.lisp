(defpackage :senn-kkc-engine.hachee.user-dict.cffi
  (:use :cl)
  (:export :with-library-loaded
           :user-dict-load
           :user-dict-destroy
           :user-dict-count
           :user-dict-entry
           :entry-form
           :entry-pron))
(in-package :senn-kkc-engine.hachee.user-dict.cffi)

(defmacro with-library-loaded ((dirs) &body body)
  (let ((g-dirs (gensym)))
    `(let ((,g-dirs ,dirs))
       (dolist (dir ,g-dirs)
         (let ((path (merge-pathnames "libuserdict.so" dir)))
           (when (uiop/filesystem:file-exists-p path)
             ;; "dont-save t" is required to load the shared object.
             ;; Otherwise, the following logs are displayed:
             ;; STYLE-WARNING: Undefined alien: "user_dict_load"
             ;; STYLE-WARNING: Undefined alien: "user_dict_destroy"
             ;; STYLE-WARNING: Undefined alien: "user_dict_count"
             ;; STYLE-WARNING: Undefined alien: "user_dict_entry"
             ;; STYLE-WARNING: Undefined alien: "entry_form"
             ;; STYLE-WARNING: Undefined alien: "entry_pron"
             (sb-alien:load-shared-object path :dont-save t))))
       (let ((cffi:*foreign-library-directories* ,g-dirs))
         (let ((handle (cffi:load-foreign-library "libuserdict.so")))
           (unwind-protect (progn ,@body)
             (cffi:close-foreign-library handle)))))))

(cffi:defcfun ("user_dict_load" user-dict-load) :pointer
  (path :string))

(cffi:defcfun ("user_dict_destroy" user-dict-destroy) :void
  (user-dict :pointer))

(cffi:defcfun ("user_dict_count" user-dict-count) :int
  (user-dict :pointer))

(cffi:defcfun ("user_dict_entry" user-dict-entry) :pointer
  (user-dict :pointer)
  (index :int))

(cffi:defcfun ("entry_form" entry-form) :string
  (entry :pointer))

(cffi:defcfun ("entry_pron" entry-pron) :string
  (entry :pointer))
