(asdf:defsystem :hachee-language-model
  :serial t
  :pathname "src/language-model/"
  :components
  ((:file "vocabulary")
   (:file "language-model")
   (:file "freq")
   (:file "n-gram"))
  :depends-on (:alexandria))
