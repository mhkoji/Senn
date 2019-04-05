(asdf:defsystem :senn
  :serial t
  :pathname "src"
  :components
  ((:file "kkc")
   (:file "segment")
   (:file "im")
   (:file "buffer"))
  :depends-on (:hachee))
