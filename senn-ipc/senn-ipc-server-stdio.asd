(asdf:defsystem :senn-ipc-server-stdio
  :serial t
  :components
  ((:file "src/server")
   (:file "src/server/stdio"))
  :depends-on (:log4cl))
