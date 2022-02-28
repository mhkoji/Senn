(asdf:defsystem :senn-server-named-pipe
  :serial t
  :components
  ((:file "src/ipc/named-pipe")
   (:file "src/server/server")
   (:file "src/server/named-pipe"))
  :depends-on (:senn
               :win32))
