(asdf:defsystem :hachee-kkc-impl-lm
  :serial t
  :pathname "src/kkc/impl/lm/"
  :components
  ((:file "unit")
   (:file "dictionary")
   (:file "dump")
   (:file "lm")
   (:file "build"))
  :depends-on (:hachee-kkc
               :hachee-language-model
               :hachee-corpus))
