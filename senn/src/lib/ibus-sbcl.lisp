(in-package :senn.lib.ibus)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sbcl-librarian:define-handle-type ime-t "ime_t")

  (sbcl-librarian:define-enum-type error-type "err_t"
    ("SENN_ERR_SUCCESS" 0)
    ("SENN_ERR_FAIL" 1))

  (sbcl-librarian:define-error-map error-map error-type 0
    (t (condition) (declare (ignore condition)) 1)))

(sbcl-librarian:define-library libsenn-ibus
    (:error-map error-map
     :function-prefix "senn_"
     :bindings-generator build-bindings
     :core-generator save-core)
  (:literal
   "#ifndef _libsenn_ibus_h"
   "#define _libsenn_ibus_h")
  (:literal
   "/* types */")
  (:type ime-t error-type)
  (:literal
   "/* functions */")
  (:function
   (make-ime ime-t ())
   (close-ime :int ((ime ime-t)))
   (handle-request :string ((ime ime-t) (req :string))))
  (:literal
   "#endif"))

(build-bindings *default-pathname-defaults*)
(save-core *default-pathname-defaults*)
