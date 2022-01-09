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
     (:file "kkc")
     (:module :im-kkc
      :pathname "kkc"
      :components
      ((:file "request")
       (:file "engine")))
     (:file "predict")
     (:module :im-predict
      :pathname "predict"
      :components
      (#+nil
       (:file "prefix")
       (:file "katakana")))
     (:file "ime"))))
  :depends-on (:cl-trie
               :alexandria
               :jsown
               #+win32
               :win32))
