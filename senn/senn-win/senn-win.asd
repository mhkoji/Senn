(asdf:defsystem :senn-win
  :serial t
  :pathname "cl"
  :components
  ((:file "keys")
   (:file "im")
   (:file "server/server")
   (:file "server/ipc"))
  :depends-on (:senn))
