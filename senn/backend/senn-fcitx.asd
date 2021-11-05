(asdf:defsystem :senn-fcitx
  :serial t
  :pathname "src/fcitx"
  :components
  ((:file "keys")
   (:file "irv")
   (:file "im/ime")
   (:file "im/process-input")
   (:file "stateful-ime")
   (:module :ipc
    :pathname "server"
    :components
    ((:file "server")
     (:file "unix")
     (:file "tcp"))))
  :depends-on (:senn
               :usocket))
