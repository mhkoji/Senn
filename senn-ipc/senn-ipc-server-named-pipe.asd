(asdf:defsystem :senn-ipc-server-named-pipe
  :serial t
  :components
  ((:file "src/server/log")
   (:file "src/server")
   (:file "src/server/named-pipe"))
  :depends-on (:bordeaux-threads
               :senn-ipc-named-pipe))
