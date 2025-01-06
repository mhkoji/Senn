(asdf:defsystem :hachee-language-model
  :serial t
  :pathname "src/language-model/"
  :components
  ((:file "vocabulary")
   (:file "ngram/probability")
   (:file "ngram/ngram")
   (:file "ngram/esitmate-weights"))
  :depends-on (:alexandria))
