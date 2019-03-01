(asdf:defsystem :senn-emacs
  :serial t
  :pathname "src"
  :components
  ((:file "kkc")
   (:file "stdio-server"))
  :depends-on (:senn))
