(asdf:defsystem :senn-emacs
  :serial t
  :pathname "src"
  :components
  ((:file "ime")
   (:file "stdio-server"))
  :depends-on (:senn))
