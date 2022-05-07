(asdf:defsystem :senn-ipc-server-named-pipe
  :serial t
  :components
  ((:file "src/server/log")
   (:file "src/server")
   (:file "src/server/named-pipe"))
  :depends-on (:senn-ipc-named-pipe
	       :log4cl))
