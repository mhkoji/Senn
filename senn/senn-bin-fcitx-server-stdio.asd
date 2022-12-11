(asdf:defsystem :senn-bin-fcitx-server-stdio
  :serial t
  :components
  ((:file "src/bin/fcitx-server-stdio"))
  :depends-on (:senn-fcitx
               :senn-im-kkc-engine
               :senn-ipc-server-stdio))
