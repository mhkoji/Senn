(asdf:defsystem :senn-fcitx-test
  :serial t
  :pathname "t/fcitx/"
  :components
  ((:file "fcitx")
   (:file "inputting")
   (:file "converting")
   (:file "hachee"))

  :depends-on (:senn-bin-fcitx-server
               :fiveam))
