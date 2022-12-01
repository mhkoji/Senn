(asdf:defsystem :senn-lib-fcitx-kkc-proxy
  :serial t
  :components
  ((:file "src/im/kkc/request")
   (:file "src/im/kkc/unix")
   (:file "src/lib/fcitx")
   (:file "src/lib/fcitx-kkc-proxy"))
  :depends-on (:senn-fcitx
               :senn-ipc-server-unix))
