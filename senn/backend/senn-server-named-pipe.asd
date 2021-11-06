(asdf:defsystem :senn-server-named-pipe
  :serial t
  :components
  ((:file "src/server/named-pipe"))
  :depends-on (:senn))
