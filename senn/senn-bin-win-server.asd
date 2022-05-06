(asdf:defsystem :senn-bin-win-server
  :serial t
  :components
  ((:file "src/bin/win-server"))
  :depends-on (:senn-win
               :senn-im-kkc-engine
               :senn-ipc-server-named-pipe))
