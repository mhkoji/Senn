(asdf:defsystem :senn-bin-fcitx-server
  :serial t
  :pathname "src"
  :components
  ((:file "../../senn-kkc/src/hachee")
   (:file "../../senn-kkc/src/store/hachee/user-dict")
   (:file "../../senn-kkc/src/store/hachee/hachee")
   (:file "bin/fcitx-server"))
  :depends-on (:hachee-kkc-impl-lm
               :hachee-data
               :senn-fcitx
               :senn-ipc-server-unix
               :senn-ipc-server-tcp))
