(asdf:defsystem :senn-bin-ibus-server
  :serial t
  :components
  ((:file "src/bin/ibus-server"))
  :depends-on (:senn-ibus
               :senn-bin-fcitx-server))
