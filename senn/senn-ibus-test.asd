(asdf:defsystem :senn-ibus-test
  :serial t
  :pathname "t/"
  :components
  ((:file "ibus/ibus")
   (:file "ibus/inputting")
   (:file "ibus/converting"))
  :depends-on (:senn-bin-ibus-server
               :fiveam))
