(asdf:defsystem :senn-ibus-test
  :serial t
  :pathname "t/"
  :components
  ((:file "base")
   (:file "fcitx-util")
   (:file "ibus/ibus")
   (:file "ibus/inputting")
   (:file "ibus/converting")
   (:file "ibus/lib"))
  :depends-on (:senn-bin-ibus-server
               :senn-lib-ibus
               :fiveam))
