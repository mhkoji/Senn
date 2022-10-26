(asdf:defsystem :senn-bin-win-server-named-pipe-tcp
  :serial t
  :components
  ((:file "src/win/server-tcp")
   (:file "src/bin/win-server-named-pipe")
   (:file "src/bin/win-server-named-pipe-tcp"))
  :depends-on (:senn-ipc-server-named-pipe))
