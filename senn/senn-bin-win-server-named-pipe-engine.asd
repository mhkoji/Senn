(asdf:defsystem :senn-bin-win-server-named-pipe-engine
  :serial t
  :components
  ((:file "src/bin/win-server-named-pipe")
   (:file "src/bin/win-server-named-pipe-engine"))
  :depends-on (:senn-win
               :senn-kkc-engine
               :senn-ipc-server-named-pipe))
