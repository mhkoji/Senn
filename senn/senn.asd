(asdf:defsystem :senn
  :serial t
  :pathname "senn"
  :components
  ((:file "op")

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
    ((:file "states")
     (:file "controller/controller")
     (:file "controller/transit")
     (:file "controller/response")
     (:file "ipc-server"))))
  :depends-on (:hachee))
