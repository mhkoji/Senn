(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src"
  :components
  ((:module :transit
    :pathname "transit"
    :components
    ((:file "keys")
     (:file "transit")
     (:file "im/states")
     (:file "im/im")))

   (:module :net
    :pathname "net"
    :components
    ((:file "net")
     (:file "ipc")
     (:file "tcp")))

   (:file "stateful-im"))
  :depends-on (:senn

               :usocket))
