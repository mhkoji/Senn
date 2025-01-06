(asdf:defsystem :hachee-language-model
  :serial t
  :pathname "src/language-model/"
  :components
  ((:file "vocabulary")
   (:file "n-gram/n-gram")
   (:file "n-gram/esitmate-weights"))
  :depends-on (:alexandria))
