(asdf:defsystem :hachee-kkc-impl-class-ngram
  :serial t
  :pathname "src/kkc/impl/class-ngram/"
  :components
  ((:file "ex-dict")
   (:file "ex-dict-builder")
   (:file "class-ngram"))
  :depends-on (:hachee-kkc
               :cl-ppcre))
