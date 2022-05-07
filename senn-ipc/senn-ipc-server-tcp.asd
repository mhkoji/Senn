(asdf:defsystem :senn-ipc-server-tcp
  :serial t
  :components
  ((:file "src/server/log")
   (:file "src/server")
   (:file "src/server/tcp"))
  :depends-on (:usocket
	       :log4cl))
