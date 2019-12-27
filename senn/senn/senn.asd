(asdf:defsystem :senn
  :serial t
  :pathname "src"
  :components
  ((:file "ja")
   (:file "buffer")
   (:file "segment")
   (:file "im/im")
   (:file "im/kkc")
   (:file "im/net/client")
   (:file "im/net/server")
   #+linux
   (:file "im/net/ipc/unix")
   (:file "im/net/stdio"))
  :depends-on (:hachee))
