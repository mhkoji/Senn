(asdf:defsystem :senn-fcitx-test
  :serial t
  :pathname "t"
  :components
  ((:file "scenario/fcitx/fcitx")
   (:file "scenario/fcitx/inputting")
   (:file "scenario/fcitx/converting")
   (:file "scenario/fcitx/hachee"))

  :depends-on (:senn-bin-fcitx-server
               :fiveam))
