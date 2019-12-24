(asdf:defsystem :senn
  :serial t
  :pathname "src"
  :components
  ((:file "ja")
   (:file "buffer")
   (:file "segment")
   (:file "im/im")
   (:file "im/kkc")
   (:file "im/net/net")
   (:file "im/net/ipc")
   (:file "im/net/stdio"))
  :depends-on (:hachee))
