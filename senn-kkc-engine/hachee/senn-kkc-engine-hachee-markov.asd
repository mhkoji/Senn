(asdf:defsystem :senn-kkc-engine-hachee-markov
  :serial t
  :components
  ((:file "user-dict")
   (:file "engine")
   (:file "markov"))
  :depends-on (:hachee-kkc-impl-markov
               :alexandria
               :jsown))
