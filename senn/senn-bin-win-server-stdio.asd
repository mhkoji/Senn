(asdf:defsystem :senn-bin-win-server-stdio
  :serial t
  :components
  ((:file "src/bin/win-server-stdio"))
  :depends-on (:senn-win
               :senn-im-kkc-engine
               :senn-ipc-server-stdio))
