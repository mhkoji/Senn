(asdf:defsystem :senn-bin-fcitx-server
  :serial t
  :pathname "src"
  :components
  ((:file "im/kkc/hachee")
   (:file "fcitx/stateful-ime-hachee")
   (:file "bin/fcitx-server"))
  :depends-on (:hachee-kkc
               :senn-fcitx
               :senn-server-unix
               :senn-server-tcp))