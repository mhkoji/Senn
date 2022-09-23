(asdf:defsystem :senn-bin-fcitx-server
  :serial t
  :pathname "src"
  :components
  ((:file "im/kkc/hachee")
   (:file "im/kkc-store/hachee/user-dict")
   (:file "im/kkc-store/hachee/hachee")
   (:file "bin/fcitx-server"))
  :depends-on (:hachee-kkc-impl-lm
               :hachee-data
               :senn-fcitx
               :senn-ipc-server-unix
               :senn-ipc-server-tcp))
