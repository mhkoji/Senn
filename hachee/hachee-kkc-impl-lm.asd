(asdf:defsystem :hachee-kkc-impl-lm
  :serial t
  :pathname "src/kkc/impl/lm/"
  :components
  ((:file "unit")
   (:file "dictionary")
   (:file "build")
   (:file "dump")
   (:file "lm"))
  :depends-on (:hachee-kkc
               :hachee-language-model
               :hachee-corpus))
