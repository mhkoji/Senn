(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/cl"
  :components
  ((:file "transit/states")
   (:file "transit/keys")
   (:file "transit/transit")
   (:file "net/net")
   (:file "stateful/im")

   (:file "net/ipc"))
  :depends-on (:senn))

