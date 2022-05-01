(asdf:defsystem :senn-kkc-engine-hachee-markov
  :serial t
  :pathname "src/engine/"
  :components
  ((:file "engine")
   (:file "markov"))
  :depends-on (:hachee-kkc-impl-markov
               :senn-kkc-engine-hachee-user-dict
               #-win32 :senn-ipc-server-stdio
               #+win32 :senn-ipc-server-named-pipe
               :alexandria
               :jsown))
