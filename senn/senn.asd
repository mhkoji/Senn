(asdf:defsystem :senn
  :serial t
  :pathname "src"
  :components
  ((:file "kkc")
   (:file "buffer")
   (:file "segment"))
  :depends-on (:hachee))
