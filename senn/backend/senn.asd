(asdf:defsystem :senn
  :serial t
  :pathname "src"
  :components
  ((:file "ja")
   (:file "buffer")
   (:file "segment")
   (:file "prefix-dictionary")
   (:file "user-dictionary")
   (:module :im
    :pathname "im/"
    :components
    ((:file "im")
     (:file "kkc")
     (:file "prefix-dictionary")
     (:file "mixin")
     (:file "net/client")
     (:file "net/server")
     #+linux
     (:file "net/ipc/unix")
     (:file "net/stdio"))))
  :depends-on (:hachee
               :cl-trie
               :cl-csv))
