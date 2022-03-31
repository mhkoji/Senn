(asdf:defsystem :senn-kkc-engine-hachee-markov
  :serial t
  :components
  ((:file "engine")
   (:file "markov"))
  :depends-on (:hachee-kkc-impl-markov
               :senn-user-dict-markov
               :alexandria
               :jsown))
