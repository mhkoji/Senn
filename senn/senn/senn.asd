(asdf:defsystem :senn
  :serial t
  :pathname "src"
  :components
  ((:file "ja")
   (:file "buffer")
   (:file "segment")
   (:file "im/im")
   (:file "im/kkc"))
  :depends-on (:hachee))
