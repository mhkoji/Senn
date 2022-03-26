(asdf:defsystem :hachee-kkc-impl-markov
  :serial t
  :pathname "src/kkc/impl/markov/"
  :components
  ((:file "cost")
   (:file "int-str")
   (:file "in-dict")
   (:file "ex-dict")
   (:file "ex-dict-builder")
   (:file "markov")
   (:file "read"))
  :depends-on (:hachee-kkc
               :cl-ppcre))
