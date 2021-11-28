(in-package :senn.lib.fcitx)

(defvar *kkc*
  (senn.im.mixin.hachee:build-kkc))

(defun make-ime ()
  (senn.fcitx.stateful-ime:make-hachee-ime *kkc*))

(defun close-ime (ime)
  (declare (ignore ime)))

(sbcl-librarian:define-handle-type ime-t "ime_t")

(sbcl-librarian:define-enum-type error-type "err_t"
  ("SENN_ERR_SUCCESS" 0)
  ("SENN_ERR_FAIL" 1))

(sbcl-librarian:define-error-map error-map error-type 0
  (t (condition) (declare (ignore condition)) 1))

(sbcl-librarian:define-library libsennfcitx
    (:error-map error-map
     :function-prefix "senn_"
     :bindings-generator build-bindings
     :core-generator save-core)
  (:literal
   "#ifndef _libsennfcitx_h"
   "#define _libsennfcitx_h")
  (:literal
   "#ifdef __cplusplus"
   "extern \"C\" {"
   "#endif")
  (:literal
   "/* types */")
  (:type ime-t error-type)
  (:literal
   "/* functions */")
  (:function
   (make-ime ime-t ())
   (handle-request :string ((ime ime-t) (req :string))))
  (:literal
   "#ifdef __cplusplus"
   "}"
   "#endif")
  (:literal
   "#endif"))
    
(build-bindings *default-pathname-defaults*)
(save-core *default-pathname-defaults*)
