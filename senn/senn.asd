(asdf:defsystem :senn
  :serial t
  :pathname "src/cl"
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
     (:file "net/client")
     (:file "net/server")
     #+linux
     (:file "net/ipc/unix")
     (:file "net/stdio")
     (:file "predictors")
     (:file "build"))))
  :depends-on (:hachee
               :cl-trie
               :cl-csv))
