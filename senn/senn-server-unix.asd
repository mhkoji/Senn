(asdf:defsystem :senn-server-unix
  :serial t
  :components
  ((:file "src/server/server")
   (:file "src/server/unix"))
  :depends-on (:senn))
