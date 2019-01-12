(asdf:defsystem :senn
  :serial t
  :pathname "senn"
  :components
  ((:file "op")
   (:file "kkc")

   (:module :emacs
    :pathname "emacs"
    :components
    ((:file "kkc")
     (:file "stdio-server")))

   (:file "ipc")
   (:file "buffer")
   (:file "segment")

   (:module :fcitx
    :pathname "fcitx"
    :components
    ((:file "states/states")
     (:file "states/view")
     (:file "keys")
     (:file "im")
     (:file "server/server")
     (:file "server/ipc"))))
  :depends-on (:hachee))
