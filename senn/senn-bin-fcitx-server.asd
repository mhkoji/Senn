(asdf:defsystem :senn-bin-fcitx-server
  :serial t
  :pathname "src"
  :components
  ((:file "im/kkc/hachee/user-dict")
   (:file "im/kkc/hachee")
   (:file "im/kkc-store/hachee")
   (:file "bin/fcitx-server"))
  :depends-on (:hachee-kkc-impl-lm
               :hachee-data
               :senn-fcitx
               :senn-server-unix
               :senn-server-tcp))
