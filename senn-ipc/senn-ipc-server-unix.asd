(asdf:defsystem :senn-ipc-server-unix
  :serial t
  :pathname "src"
  :components
  ((:file "ipc/unix")
   (:file "server/log")
   (:file "server")
   (:file "server/unix"))
  :depends-on (:log4cl))
