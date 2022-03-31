(asdf:defsystem :senn-bin-fcitx-server
  :serial t
  :pathname "src"
  :components
  ((:file "im/kkc/hachee")
   (:file "im/kkc-store/hachee")
   (:file "bin/fcitx-server"))
  :depends-on (:hachee-kkc-impl-lm
               :hachee-data
               :senn-user-dict-lm
               :senn-fcitx
               :senn-server-unix
               :senn-server-tcp))
