(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/cl/fcitx"
  :components
  ((:module :input-processor
    :pathname "input-processor"
    :components
    ((:file "keys")
     (:file "input-processor")
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
               :senn-gui

               :usocket))
