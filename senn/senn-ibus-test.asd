(asdf:defsystem :senn-ibus-test
  :serial t
  :pathname "t/"
  :components
  ((:file "base")
   (:file "im-util")
   (:file "ibus/ibus")
   (:file "ibus/inputting")
   (:file "ibus/converting")
   (:file "ibus/lib"))
  :depends-on (:senn-lib-ibus
               :fiveam))
