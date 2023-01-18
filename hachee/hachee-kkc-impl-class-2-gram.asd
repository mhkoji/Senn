(asdf:defsystem :hachee-kkc-impl-class-2-gram
  :serial t
  :pathname "src/kkc/impl/class-2-gram/"
  :components
  ((:file "ex-dict")
   (:file "ex-dict-builder")
   (:file "class-2-gram"))
  :depends-on (:hachee-kkc
               :cl-ppcre))
