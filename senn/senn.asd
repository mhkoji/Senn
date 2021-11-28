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
    :pathname "im"
    :components
    ((:file "im")
     (:module :mixin
      :pathname "mixin"
      :components
      ((:file "hachee")
       (:file "engine")
       (:file "prefix")
       (:file "katakana"))))))
  :depends-on (:hachee-kkc
               :cl-trie
               :cl-csv))
