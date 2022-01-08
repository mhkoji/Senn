(asdf:defsystem :senn
  :serial t
  :pathname "src"
  :components
  ((:file "ja")
   (:file "prefix-dictionary")
   (:module :ipc
    :pathname "ipc"
    :components
    (#+linux
     (:file "unix")
     #+win32
     (:file "named-pipe")))
   (:module :im
    :pathname "im"
    :components
    ((:file "buffer")
     (:file "segment")
     (:file "ime")
     (:module :kkc
      :pathname "kkc"
      :components
      ((:file "engine")))
     (:module :predict
      :pathname "predict"
      :components
      (#+nil
       (:file "prefix")
       (:file "katakana"))))))
  :depends-on (:cl-trie
               :alexandria
               :jsown
               #+win32
               :win32))
