(asdf:defsystem :senn-lib-fcitx
  :serial t
  :components
  ((:file "src/lib/fcitx")
   (:file "src/lib/fcitx-engine"))
  :depends-on (:senn-fcitx
               :senn-im-kkc-engine))
