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
     #+sbcl
     (:file "engine")
     (:file "prefix-dictionary")
     (:file "mixin"))))
  :depends-on (:hachee
               :cl-trie
               :cl-csv))
