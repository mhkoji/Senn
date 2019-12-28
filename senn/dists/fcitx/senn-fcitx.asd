(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/cl"
  :components
  ((:module :transit
    :pathname "transit"
    :components
    ((:file "keys")
     (:file "transit")
     (:file "im/states")
     (:file "im/im")))

   (:file "net/net")
   (:file "stateful/im")

   (:file "net/ipc"))
  :depends-on (:senn))
