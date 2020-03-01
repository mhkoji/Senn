(asdf:defsystem :senn
  :serial t
  :pathname "src"
  :components
  ((:file "ja")
   (:file "buffer")
   (:file "segment")
   (:file "prefix-dictionary")
   (:module :im
    :pathname "im/"
    :components
    ((:file "im")
     (:file "kkc")
     (:file "net/client")
     (:file "net/server")
     #+linux
     (:file "net/ipc/unix")
     (:file "net/stdio")
     (:file "predict"))))
  :depends-on (:hachee
               :cl-trie))
