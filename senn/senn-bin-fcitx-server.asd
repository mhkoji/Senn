(asdf:defsystem :senn-bin-fcitx-server
  :serial t
  :pathname "src"
  :components
  ((:file "bin/fcitx-server"))
  :depends-on (:hachee-kkc-impl-lm
               :hachee-data
               :senn-fcitx
               :senn-kkc-hachee
               :senn-ipc-server-unix
               :senn-ipc-server-tcp))
