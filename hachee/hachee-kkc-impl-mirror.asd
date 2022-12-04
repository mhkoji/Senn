(asdf:defsystem :hachee-kkc-impl-mirror
  :serial t
  :pathname "src/kkc/impl/mirror/"
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
