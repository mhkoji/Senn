(asdf:defsystem :senn-server-unix
  :serial t
  :pathname "src"
  :components
  ((:file "ipc/unix")
   (:file "server/server")
   (:file "server/unix"))
  :depends-on (:jsown))
