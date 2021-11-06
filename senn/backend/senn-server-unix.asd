(asdf:defsystem :senn-server-unix
  :serial t
  :components
  ((:file "src/server/unix"))
  :depends-on (:senn))
