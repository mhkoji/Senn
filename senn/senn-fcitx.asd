(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/cl/fcitx"
  :components
  ((:module :transit
    :pathname "transit"
    :components
    ((:file "keys")
     (:file "transit")
     (:file "im/states")
     (:file "im/im")))

   (:module :ipc
    :pathname "ipc"
    :components
    ((:file "ipc")
     (:file "unix")
     (:file "tcp")))

   (:file "stateful-im"))
  :depends-on (:senn

               :usocket))
