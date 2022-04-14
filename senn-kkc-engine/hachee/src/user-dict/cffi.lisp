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

(defmacro with-library-loaded (&body body)
  `(let ((handle (cffi:load-foreign-library "libuserdict.so")))
     (unwind-protect (progn ,@body)
       (cffi:close-foreign-library handle))))

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
