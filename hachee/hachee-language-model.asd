(asdf:defsystem :hachee-language-model
  :serial t
  :pathname "src/language-model/"
  :components
  ((:file "vocabulary")
   (:file "n-gram"))
  :depends-on (:alexandria))
