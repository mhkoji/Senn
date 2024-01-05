(asdf:defsystem :senn-bin-fcitx-server
  :serial t
  :pathname "src"
  :components
  ((:file "bin/fcitx-server"))
  :depends-on (:senn-fcitx
               :senn-im-kkc-hachee
               :senn-im-kkc-engine
               :senn-ipc-server-unix
               :senn-ipc-server-tcp))
