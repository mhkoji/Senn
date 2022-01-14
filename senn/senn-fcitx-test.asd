(asdf:defsystem :senn-fcitx-test
  :serial t
  :pathname "t"
  :components
  ((:file "scenario/fcitx/fcitx")
   (:file "scenario/fcitx/inputting")
   (:file "scenario/fcitx/converting"))

  :depends-on (:senn-fcitx
               :fiveam))
