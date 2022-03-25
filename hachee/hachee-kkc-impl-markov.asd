(asdf:defsystem :hachee-kkc-impl-markov
  :serial t
  :pathname "src/kkc/impl/markov/"
  :components
  ((:file "int-str")
   (:file "in-dict")
   (:file "ex-dict")
   (:file "markov")
   (:file "read"))
  :depends-on (:hachee-kkc
               :cl-ppcre))
