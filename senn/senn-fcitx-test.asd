(asdf:defsystem :senn-fcitx-test
  :serial t
  :pathname "t/"
  :components
  ((:file "fcitx-util")
   (:module :fcitx
    :pathname "fcitx/"
    :components
    ((:file "fcitx")
     (:file "inputting")
     (:file "converting")
     (:file "hachee")
     (:file "lib"))))
  :depends-on (:senn-lib-fcitx
               :senn-bin-fcitx-server
               :fiveam))
