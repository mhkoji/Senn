(asdf:defsystem :senn-kkc-engine-hachee-markov
  :serial t
  :pathname "src/engine/"
  :components
  ((:file "engine")
   (:file "markov"))
  :depends-on (:hachee-kkc-impl-markov
               :senn-kkc-engine-hachee-user-dict
	       :senn-ipc-server-stdio
               :alexandria
               :jsown))
