(asdf:defsystem :senn-fcitx-test
  :serial t
  :pathname "t"
  :components
  ((:file "scenario/fcitx/fcitx")
   (:file "scenario/fcitx/inputing")
   (:file "scenario/fcitx/converting"))

  :depends-on (:senn-fcitx
               :fiveam))
