(asdf:defsystem :senn
  :serial t
  :pathname "src"
  :components
  ((:file "ja")
   (:file "buffer")
   (:file "segment")
   (:module :im
    :pathname "im/"
    :components
    ((:file "im")
     (:file "kkc")
     (:file "net/client")
     (:file "net/server")
     #+linux
     (:file "net/ipc/unix")
     (:file "net/stdio"))))
  :depends-on (:hachee))
